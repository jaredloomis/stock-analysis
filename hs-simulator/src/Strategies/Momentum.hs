module Strategies.Momentum where

import RIO
import RIO.List (lastMaybe)

import Data.Maybe (fromJust, isJust)
import Data.Time.Clock (addUTCTime, diffUTCTime, nominalDiffTimeToSeconds, UTCTime(..), DiffTime)
import Data.List (scanl', splitAt)

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

associatedMomentum :: Text -> Text -> Int -> Int -> StrategyM ()
associatedMomentum watchTicker tradeTicker days weight = do
  samplesByDay <- dailySamples "price" watchTicker days
  if isAscending samplesByDay
    then trade tradeTicker Buy weight
    else if isAscending (reverse samplesByDay)
      then trade tradeTicker Sell weight
      else return ()

tradeMovingAverage :: Text -> Int -> Int -> Int -> StrategyM ()
tradeMovingAverage ticker days averageSpan buySellCount = do
  samplesByDay <- dailySamples "price" ticker days
  avgs <- movingAverage ticker days averageSpan
  let avg = lastMaybe avgs
  if isAscending avgs && isAscending samplesByDay
    then trade ticker Buy buySellCount
    else if isAscending (reverse avgs) && isAscending samplesByDay
      then trade ticker Sell buySellCount
      else pure ()

movingAverage :: Text -> Int -> Int -> StrategyM [Double]
movingAverage ticker days averageSpan = do
  samplesByDay <- dailySamples "price" ticker days
  trace "AYYY SAMPLES BY DAY: =================" (pure ())
  traceShow samplesByDay (pure ())
  pure . mavg averageSpan . map sampleValue $ samplesByDay

isAscending (x:y:xs)
  | x <= y     = isAscending xs
  | otherwise = False
isAscending _ = True

slope distanceX a b =
  let distanceX = realToFrac . nominalDiffTimeToSeconds $ diffUTCTime (sampleStartTime a) (sampleStartTime b) :: Double
      (ax, ay) = (0, sampleValue a)
      (bx, by) = (distanceX, sampleValue b)
  in (by - ay) / (bx - ax)

-- | Moving average
mavg :: Fractional b => Int -> [b] -> [b]
mavg k lst = map (/ fromIntegral k) $ scanl' (+) (sum h) $ zipWith (-) t lst
  where (h, t) = splitAt k lst 

zigZagDaily :: Text -> Double -> Int -> StrategyM [IndicatorSample]
zigZagDaily ticker maxChange days = do
  samplesByDay <- take days <$> dailySamples "price" ticker days
  let initialPivotPoint = listToMaybe samplesByDay
  if isJust initialPivotPoint
    then
        let (_, _, _, changePoints) = foldr zigZag' (Nothing, Nothing, Nothing, []) samplesByDay
        in pure changePoints
    else trace "zigZag: NO DATA ON THIS ITERATION. EXITING" (pure [])
 where
  zigZag' :: IndicatorSample -> (Maybe Bool, Maybe IndicatorSample, Maybe IndicatorSample, [IndicatorSample]) -> (Maybe Bool, Maybe IndicatorSample, Maybe IndicatorSample, [IndicatorSample])
  zigZag' sample (currentDirection, currentPeakTrough, lastSample, acc) =
    let value              = sampleValue sample
        isSwitchPoint      =
          if isNothing currentPeakTrough || isNothing currentDirection
            then True
            else
              let peakTrough  = sampleValue $ fromJust currentPeakTrough
                  dir         = fromJust currentDirection
                  distFromRef = value - peakTrough
                  change      = distFromRef / peakTrough
              in (distFromRef > 0) /= dir && change >= maxChange
        (currentDirection', currentPeakTrough', acc) =
          if isSwitchPoint
            then (
              fmap (sample >) currentPeakTrough',
              Just sample,
              sample : acc
              )
            else (
              currentDirection,
              currentPeakTrough <|> lastSample <|> Just sample,
              acc
              )
    in (currentDirection', currentPeakTrough', Just sample, acc)
