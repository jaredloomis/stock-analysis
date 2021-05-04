module Strategy where

import RIO
import RIO.Time (addUTCTime)

import Control.Monad.Free (Free(..), liftF)
import Data.Text (Text)
import Data.Maybe (fromJust, isJust)
import Data.Time.Clock (UTCTime(..), NominalDiffTime, addUTCTime)

import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.Vector as V

import IndicatorSample
import IndicatorArgs
import Time

type StrategyM = Free StrategyF

data StrategyF next where
  FetchSamples :: !Text -> !IndicatorArgs        -> !([IndicatorSample]     -> next) -> StrategyF next
  FetchSample  :: !Text -> !IndicatorArgs        -> !(Maybe IndicatorSample -> next) -> StrategyF next
  Trade        :: !Text -> !TradeType     -> Int -> !next                            -> StrategyF next
  GetTime      ::                                   !(UTCTime -> next)               -> StrategyF next
  deriving (Functor)

instance Show (StrategyF next) where
  show (FetchSamples sym args _)   = "FetchSamples(" ++ show sym ++ ", " ++ show args ++ ")"
  show (FetchSample  sym args _)   = "FetchSample("  ++ show sym ++ ", " ++ show args ++ ")"
  show (Trade        sym ty qty _) = "Trade(" ++ show sym ++ ", " ++ show ty ++ ", " ++ show qty ++ ")"
  show (GetTime _)                 = "GetTime"

data TradeType = Buy | Sell | Put !UTCTime | Call !UTCTime
  deriving (Show, Eq)

fetchSamples :: Text -> IndicatorArgs -> StrategyM [IndicatorSample]
fetchSamples indicatorID args = liftF (FetchSamples indicatorID args id)

fetchSample :: Text -> IndicatorArgs -> StrategyM (Maybe IndicatorSample)
fetchSample indicatorID args = liftF (FetchSample indicatorID args id)

trade :: Text -> TradeType -> Int -> StrategyM ()
trade stockID ty count = liftF (Trade stockID ty count ())

getTime :: StrategyM UTCTime
getTime = liftF (GetTime id)

dailySamples :: Text -> Text -> Int -> StrategyM [IndicatorSample]
dailySamples indicatorID ticker days =
  map fromJust . filter isJust <$> mapM (daySample indicatorID ticker) [0..(-days)]

--sampleEvery :: Text -> Text -> NominalDiffTime -> StrategyM [IndicatorSample]
--sampleEvery indicatorID ticker deltaTime =

daySample :: Text -> Text -> Int -> StrategyM (Maybe IndicatorSample)
daySample indicatorID ticker dayDelta = do
  curTime <- getTime
  let dayTime = addUTCTime (fromIntegral $ 60*60*24*dayDelta) curTime
  let (startTime, endTime) = dayBoundaries dayTime
  let args = indicatorArgs (IndicatorTime startTime, IndicatorTime endTime) [
          ("tickers", arrayArg [A.String ticker])
        ]
  fetchSample indicatorID args

dayBoundaries :: UTCTime -> (UTCTime, UTCTime)
dayBoundaries (UTCTime day time) = (UTCTime day 0, UTCTime day (fromIntegral $ 60*60*24))

data StrategyResult = StrategyResult
  {-# UNPACK #-} !Double
                 ![TradeLog]
  deriving (Show, Eq)

data TradeLog = TradeLog
  -- | Trade type
                 !TradeType
  -- | Ticker
                 !Text
  -- | Number of assets to trade
                 !Int
  -- | Time of trade
  {-# UNPACK #-} !UTCTime
  -- | Value of each asset at time of trade (ie. total price of trade is value*count)
  {-# UNPACK #-} !Double
  deriving (Show, Eq)
