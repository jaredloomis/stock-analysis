{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.IO
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Options.Applicative
import Data.Maybe (isJust, fromJust, fromMaybe)
import Options.Applicative.Builder
import Data.Aeson (ToJSON, encode)
import Data.List.Split (chunksOf)
import Control.Monad (sequence)
import Data.Time.Clock (UTCTime(..), utctDay)
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)

import qualified FinnhubAPI as FinnhubAPI
import qualified FinnhubLocal as FinnhubLocal
import qualified Schedule as Schedule
import qualified IndicatorConfig as IndicatorConfig
import Quote (Quote(..))
import IndicatorConfig (IndicatorConfig(..), IndicatorTime(..), DataSourceSpec)
import Error (StockError(..))

import Debug.Trace (trace)

main :: IO ()
main = do
  -- Parse request
  req@(CLIRequest indicator time batchSize tickers) <- execParser parserInfo

  let showResult x =
        case x of
          Left err  -> A.String . T.pack . show $ err
          Right res -> A.toJSON res
  let printResults = B.putStrLn . encode . V.fromList . map showResult

  -- Execute request
  let tickerChunks = fmap (chunksOf batchSize) tickers
  case tickerChunks of
    Just chunks ->
      (flip mapM_) chunks $ \chunk -> do
        printResults =<< execRequest req{ reqTickersRaw = (Just chunk) }
    Nothing ->
      printResults =<< execRequest req

execRequest :: CLIRequest -> IO [Either StockError A.Value]
execRequest (CLIRequest indicator timestr _ (Just tickers)) | "price" `T.isPrefixOf` indicator =
  fromMaybe finnhubApiPrice . fmap execByIndicatorSubId $ msubIndicator
 where
  (_, indicatorParts) = parseIndicator indicator
  msubIndicator = if length indicatorParts >= 1 then Just (indicatorParts !! 0) else Nothing

  execByIndicatorSubId :: Text -> IO [Either StockError A.Value]
  execByIndicatorSubId dataSourceID
    | dataSourceID == FinnhubLocal.dataSourceID = finnhubLocalPrice
    | otherwise                                 = finnhubApiPrice

  finnhubLocalPrice :: IO [Either StockError A.Value]
  finnhubLocalPrice = case date of
    Just dateTime -> do
      result <- FinnhubLocal.quoteMany (utctDay dateTime) tickers --mapM (fmap (fmap fst) . FinnhubLocal.quote (utctDay dateTime)) tickers
      return . map (fmap A.toJSON) . sequence $ result
    Nothing -> do
      hPutStrLn stderr "Must provide timestamp for indicator source 'price-finnhub_local'"
      return []

  finnhubApiPrice :: IO [Either StockError A.Value]
  finnhubApiPrice = do
    results <- FinnhubAPI.quoteMany finnhubToken tickers
    return $ map (bimap StockApiError A.toJSON) results

  date :: Maybe UTCTime
  date = parseTime timestr
execRequest (CLIRequest indicator timestr _ (Just tickers)) | "sentiment" `T.isPrefixOf` indicator = do
  results <- FinnhubAPI.newsSentimentMany finnhubToken tickers
  return $ map (bimap StockApiError A.toJSON) results
execRequest (CLIRequest indicator timestr _ tickers) = return [Left . StockError . T.pack $
    "unsupported operation. Indicator: " ++ show indicator ++ " args: " ++ show tickers
  ]

availableSources :: FilePath -> T.Text -> Maybe IndicatorTime -> IO [DataSourceSpec]
availableSources configFilePath indicatorPrefix mtime = do
  -- Parse all sources
  IndicatorConfig configMap <- IndicatorConfig.loadIndicatorConfig configFilePath
  -- Find appropriate sources:
  -- 1. Available at specified time (if any).
  -- 2. Implemented in this package. TODO TODO TODO
  let allSources = IndicatorConfig.groupSources $ configMap M.! indicatorPrefix
  let availSources = maybe (M.elems allSources) (\time -> filter (`Schedule.isAvailableAt` time) . M.elems $ allSources) mtime
  return availSources

----

parseIndicator :: Text -> (Text, [Text])
parseIndicator text = (T.takeWhile (/= '-') text, tail $ T.splitOn "-" text)

parseTime :: Text -> Maybe UTCTime
parseTime = formatParseM (utcTimeFormat dayFormat timeFormat) . T.unpack
 where
  dayFormat  = calendarFormat ExtendedFormat
  timeFormat = timeOfDayFormat ExtendedFormat

--
-- Config
--

finnhubToken :: Text
finnhubToken = "bt8s7o748v6o22d1k1jg" --"bt8plj748v6qh9e49bu0" --"bt0vb8n48v6qbloltab0"

simFinToken :: Text
simFinToken = "IY6lhcssERR4yPTO3QilYxqTpyIV7ylq"

--
-- Arg parsing
--

data CLIRequest = CLIRequest {
  reqIndicator  :: Text,
  reqTimeRaw    :: Text,
  -- Meta
  reqBatchSize  :: Int,
  reqTickersRaw :: Maybe [Text]
} deriving (Show, Eq)

parserInfo :: ParserInfo CLIRequest
parserInfo = info cliRequest (progDesc "Stock Data")

cliRequest :: Parser CLIRequest
cliRequest = CLIRequest
  <$> strOption
      (  long "indicator"
      <> short 'i'
      <> metavar "INDICATOR"
      <> help "Indicator to run" )
  <*> strOption
      (  long "time"
      <> short 't'
      <> metavar "TIMESTAMP"
      <> showDefault
      <> value ""
      <> help "Request indicator measurement at a point in time" )
  <*> option auto
        (  long "batchSize"
        <> metavar "BATCH_SIZE"
        <> showDefault
        <> value 30
        <> help "Number of samples to output at a time" )
  <*> fmap Just stockTickers

stockTickers :: Parser [Text]
stockTickers = T.split (== ',') <$> argument str (metavar "TICKERS...")
