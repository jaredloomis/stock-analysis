{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Options.Applicative
import Options.Applicative.Builder
import Data.Aeson (ToJSON, encode)
import Data.List.Split (chunksOf)
import Control.Monad (sequence)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)

import qualified FinnhubAPI as Finnhub
import Error (StockError(..))

main :: IO ()
main = do
  -- Parse request
  req@(CLIRequest indicator batchSize tickers) <- execParser parserInfo

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
        printResults =<< execIndicator indicator (Just chunk)
    Nothing ->
      printResults =<< execIndicator indicator Nothing

execIndicator :: T.Text -> Maybe [T.Text] -> IO [Either StockError A.Value]
execIndicator "price" (Just tickers) = do
  results <- Finnhub.quoteMany finnhubToken $ tickers
  return $ map (bimap StockApiError A.toJSON) results
execIndicator "sentiment" (Just tickers) = do
  results <- Finnhub.newsSentimentMany finnhubToken $ tickers
  return $ map (bimap StockApiError A.toJSON) results
execIndicator indicator tickers = return [Left . StockError . T.pack $
  "unsupported operation. Indicator: " ++ show indicator ++ " args: " ++ show tickers]

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
  <*> option auto
      (  long "batchSize"
      <> metavar "BATCH_SIZE"
      <> showDefault
      <> value 30
      <> help "Number of samples to output at a time" )
  <*> fmap Just stockTickers

stockTickers :: Parser [Text]
stockTickers = T.split (== ',') <$> argument str (metavar "TICKERS...")
