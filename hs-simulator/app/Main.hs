module Main where

import Prelude (print, putStrLn)
import RIO
import RIO.Time (addUTCTime, getCurrentTime)
import DataLoader
import Strategy
import Time
import Summary
import Strategies.Momentum

import qualified Data.Text as T
import qualified Data.Text.IO as T

allTickers = ["FXI", "VGX", "SCHH", "ABBV", "EFA", "CTEC", "V", "MSFT", "F", "TSLA", "AMD", "SPY"]

main :: IO ()
main = cachingTest
  {-}
  let Just time = parseTime "2009-12-30T05:01:00Z"
  let durationDays = 120
  forM_ allTickers $ \tickerA -> forM_ allTickers $ \tickerB -> if tickerA == tickerB then pure () else do
    tradeLog <- executeStrategyDaily (IndicatorTime time, IndicatorTime $ addUTCTime (fromIntegral $ 60*60*24*durationDays) time) CachingDataLoader (associatedMomentum tickerA tickerB 3 12)
    putStrLn $ show tickerA ++ " - " ++ show tickerB ++ " ----------------"
    print tradeLog
    let summary = summarize tradeLog
    print summary
    putStrLn $ "Total profit: $" ++ show (totalValue summary)-}

main2 :: IO ()
main2 = do
  let Just time = parseTime "2009-12-30T05:01:00Z"
  let durationDays = 20
  tradeLog <- executeStrategyDaily (IndicatorTime time, IndicatorTime $ addUTCTime (fromIntegral $ 60*60*24*durationDays) time) naiveDataLoader (momentum "AAPL" 3 12)
  print tradeLog
  let summary = summarize tradeLog
  putStrLn $ show "AAPL" ++ " ----------------"
  print summary
  putStrLn $ "Total profit: $" ++ show (totalValue summary)

cachingTest = do
  let Just time = parseTime "2009-12-30T05:01:00Z"
  let durationDays = 30
  let ticker = "AAPL"
  tradeLog <- executeStrategyDaily (IndicatorTime time, IndicatorTime $ addUTCTime (fromIntegral $ 60*60*24*durationDays) time) CachingDataLoader (momentum ticker 3 1)
  putStrLn $ tradeLog `seq` show ticker ++ " ----------------"
  print tradeLog
  let summary = summarize tradeLog
  print summary
  putStrLn $ "Total profit: $" ++ show (totalValue summary)