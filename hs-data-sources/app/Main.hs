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
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)

import qualified FinnhubAPI as FinnhubAPI
import qualified FinnhubLocal as FinnhubLocal
import Error (StockError(..))

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
  let (_, indicatorParts) = parseIndicator indicator
      msubIndicator = if length indicatorParts >= 1 then Just (indicatorParts !! 0) else Nothing
  in fromMaybe finnhubApiPrice . fmap execByIndicatorSubId $ msubIndicator
 where
  execByIndicatorSubId :: Text -> IO [Either StockError A.Value]
  execByIndicatorSubId "finnhub_local" = finnhubLocalPrice
  execByIndicatorSubId _               = finnhubApiPrice

  finnhubLocalPrice :: IO [Either StockError A.Value]
  finnhubLocalPrice = if isJust date
    then do
      results <- mapM (FinnhubLocal.quoteMany (fromJust date)) tickers
      return . pure . fmap (A.toJSON . concat) . sequence $ results
    else
      return []

  finnhubApiPrice :: IO [Either StockError A.Value]
  finnhubApiPrice = do
    results <- FinnhubAPI.quoteMany finnhubToken tickers
    return $ map (bimap StockApiError A.toJSON) results

  date :: Maybe Day
  date = parseTime timestr
execRequest (CLIRequest indicator timestr _ (Just tickers)) | "sentiment" `T.isPrefixOf` indicator = do
  results <- FinnhubAPI.newsSentimentMany finnhubToken tickers
  return $ map (bimap StockApiError A.toJSON) results
execRequest (CLIRequest indicator timestr _ tickers) = return [Left . StockError . T.pack $
    "unsupported operation. Indicator: " ++ show indicator ++ " args: " ++ show tickers
  ]

parseIndicator :: Text -> (Text, [Text])
parseIndicator text = (T.takeWhile (/= '-') text, tail $ T.splitOn "-" text)

parseTime :: Text -> Maybe Day
parseTime = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) . T.unpack

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
