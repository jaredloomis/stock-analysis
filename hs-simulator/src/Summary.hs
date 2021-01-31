module Summary where

import RIO

import qualified Data.Map as M

import Strategy

type TradeSummary = M.Map TradeSummaryAsset TradeSummaryValue

data TradeSummaryAsset = ProfitLoss | OwnedAsset !Text
  deriving (Show, Eq, Ord)
data TradeSummaryValue = TradeSummaryValue !Double !Int
  deriving (Show, Eq)

instance Semigroup TradeSummaryValue where
  TradeSummaryValue va ca <> TradeSummaryValue vb cb = TradeSummaryValue (va + vb) (ca + cb)

summarize :: [TradeLog] -> TradeSummary
summarize = foldr insertLog mempty
  where
    insertLog (TradeLog Buy ticker count time value) acc =
      let assetKey  = OwnedAsset ticker
          totalVal  = fromIntegral count * value
          assetVal  = TradeSummaryValue value count
          profitVal = TradeSummaryValue (-totalVal) 1
      in M.insertWith addSummaryValuePrice ProfitLoss profitVal .
         M.insertWith addSummaryValueCount assetKey assetVal
         $ acc
    insertLog (TradeLog Sell ticker count time value) acc =
      let assetKey  = OwnedAsset ticker
          totalVal  = fromIntegral count * value
          assetVal  = TradeSummaryValue value (-count)
          profitVal = TradeSummaryValue totalVal 1
      in M.insertWith addSummaryValuePrice ProfitLoss profitVal .
         M.insertWith addSummaryValueCount assetKey assetVal
         $ acc

totalValue :: TradeSummary -> Double
totalValue summary = M.foldr (\(TradeSummaryValue v c) acc -> acc + v*fromIntegral c) 0 summary

addSummaryValueCount :: TradeSummaryValue -> TradeSummaryValue -> TradeSummaryValue
addSummaryValueCount (TradeSummaryValue v a) (TradeSummaryValue _ b) = TradeSummaryValue v (a + b)

addSummaryValuePrice :: TradeSummaryValue -> TradeSummaryValue -> TradeSummaryValue
addSummaryValuePrice (TradeSummaryValue a c) (TradeSummaryValue b _) = TradeSummaryValue (a + b) c

filterMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
filterMap ff mf = map (\x -> if ff x then mf x else x)

hasTicker :: TradeSummaryAsset -> Text -> Bool
hasTicker ProfitLoss           _       = False
hasTicker (OwnedAsset tickerA) tickerB = tickerA == tickerB