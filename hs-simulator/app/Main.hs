module Main where

import Prelude (print, putStrLn)
import RIO
import RIO.Time (addUTCTime, getCurrentTime)

import Control.Monad.State (runStateT)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HM

import DataLoader
import Strategy
import Time
import Summary
import Strategies.Momentum
import IndicatorSample

allTickers = ["FXI", "VGX", "SCHH", "ABBV", "EFA", "CTEC", "V", "MSFT", "F", "TSLA", "AMD", "SPY"]

main :: IO ()
main = cachingTest

zigZagTest :: IO ()
zigZagTest = do
  let Just time = parseTime "2010-01-03T00:00:00Z"
  let days = 20
  let maxChange = 0.05
  let ticker = "AMD"
  let strategy = zigZagDaily ticker maxChange days >>= flip traceShow (pure ())
  pure ()
  --void $ executeStrategyPointInTime (IndicatorTime time) CachingDataLoader (Left strategy) strategy

maTest = do
  let Just time = parseTime "2010-04-01T00:00:00Z"
  let durationDays = 10
  let ticker = "AMD"
  let strategy = tradeMovingAverage ticker 3 7 12
  --queries <- scanStrategy strategy (fromInteger $ 60*60*24) (IndicatorTime time, IndicatorTime $ addUTCTime (fromIntegral $ 60*60*24*durationDays) time)
  --let queries' = joinQueryConfigs queries
  (tradeLog, st) <- runStateT (executeStrategyDaily (IndicatorTime time, IndicatorTime $ addUTCTime (fromIntegral $ 60*60*24*durationDays) time) CachingDataLoader strategy) (mempty :: HM.HashMap DataLoaderQuery [IndicatorSample])
  putStrLn $ tradeLog `seq` show ticker ++ " ----------------"
  print tradeLog
  let summary = summarize tradeLog
  print summary
  putStrLn $ "Total value: $" ++ show (totalValue summary)

cachingTest = do
  let Just time = parseTime "2018-03-01T00:00:00Z"
  let durationDays = 100
  let ticker = "BTC"
  let strategy = momentum ticker 3 12
  --queries <- scanStrategy strategy (fromInteger $ 60*60*24) (IndicatorTime time, IndicatorTime $ addUTCTime (fromIntegral $ 60*60*24*durationDays) time)
  --let queries' = joinQueryConfigs queries
  (tradeLog, st) <- runStateT (executeStrategyDaily (IndicatorTime time, IndicatorTime $ addUTCTime (fromIntegral $ 60*60*24*durationDays) time ) CachingDataLoader strategy) (mempty :: HM.HashMap DataLoaderQuery [IndicatorSample])
  putStrLn $ tradeLog `seq` show ticker ++ " ----------------"
  print tradeLog
  let summary = summarize tradeLog
  print summary
  putStrLn $ "Total value: $" ++ show (totalValue summary)