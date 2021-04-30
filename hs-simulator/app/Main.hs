module Main where

import Prelude (print, putStrLn)
import RIO
import RIO.Time (addUTCTime, getCurrentTime)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import DataLoader
import Strategy
import Time
import Summary
import Strategies.Momentum

allTickers = ["FXI", "VGX", "SCHH", "ABBV", "EFA", "CTEC", "V", "MSFT", "F", "TSLA", "AMD", "SPY"]

main :: IO ()
main = cachingTest

cachingTest = do
  let Just time = parseTime "2010-01-03T00:00:00Z"
  let durationDays = 2
  let ticker = "AAPL"
  let strategy = momentum ticker 2 12
  tradeLog <- executeStrategyDaily (IndicatorTime time, IndicatorTime $ addUTCTime (fromIntegral $ 60*60*24*durationDays) time) NaiveDataLoader (Left strategy) strategy
  putStrLn $ tradeLog `seq` show ticker ++ " ----------------"
  print tradeLog
  let summary = summarize tradeLog
  print summary
  putStrLn $ "Total value: $" ++ show (totalValue summary)