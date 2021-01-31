module Strategies.Momentum where

import RIO

import Data.Time.Clock (addUTCTime)

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import qualified Data.Vector as V

import Strategy
import Time
import IndicatorArgs
import IndicatorSample

-- Momentum trading
-- > buy(SPY, limit=100)  when price(SPY) > price(SPY, at=now-2days) > price(SPY, at=now-4days)
-- > sell(SPY) when price(SPY) < price(SPY, at=now-2days) < price(SPY, at=now-4days)
momentum :: Text -> Int -> Int -> StrategyM ()
momentum ticker days weight = associatedMomentum ticker ticker days weight

-- Momentum trading
-- > buy(SPY, limit=100)  when price(SPY) > price(SPY, at=now-2days) > price(SPY, at=now-4days)
-- > sell(SPY) when price(SPY) < price(SPY, at=now-2days) < price(SPY, at=now-4days)
associatedMomentum :: Text -> Text -> Int -> Int -> StrategyM ()
associatedMomentum watchTicker tradeTicker days weight = do
  pricesByDay <- mapM fetchPriceSampleValue [0..days-1]
  if isAscending pricesByDay
    then trade tradeTicker Buy weight
    else if isAscending (reverse pricesByDay)
      then trade tradeTicker Sell weight
      else return ()
 where
  fetchPriceSampleValue dayDelta = do
    curTime <- getTime
    let time = IndicatorTime $ addUTCTime (fromIntegral $ 60*60*24*dayDelta) curTime
    let args = IndicatorArgs (time, time) (HM.fromList [
                  ("tickers", Any . A.Array . V.fromList $ [A.String watchTicker])
                ])
    maybe 0 sampleValue <$> fetchSample "price" args

  isAscending (x:y:xs)
    | x < y     = isAscending xs
    | otherwise = False
  isAscending _ = True
