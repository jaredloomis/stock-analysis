module Strategy where

import RIO
import RIO.Time (addUTCTime)

import Control.Monad.Free (Free(..), liftF)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

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

data TradeType = Buy | Sell | Put {-# UNPACK #-} !UTCTime | Call {-# UNPACK #-} !UTCTime
  deriving (Show, Eq)

fetchSamples :: Text -> IndicatorArgs -> StrategyM [IndicatorSample]
fetchSamples indicatorID args = liftF (FetchSamples indicatorID args id)

fetchSample :: Text -> IndicatorArgs -> StrategyM (Maybe IndicatorSample)
fetchSample indicatorID args = liftF (FetchSample indicatorID args id)

trade :: Text -> TradeType -> Int -> StrategyM ()
trade stockID ty count = liftF (Trade stockID ty count ())

getTime :: StrategyM UTCTime
getTime = liftF (GetTime id)

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
