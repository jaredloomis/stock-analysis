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
  req <- execParser parserInfo

  -- Execute request
  results <- execReq req
  
  let showResult x =
        case x of
          Left err  -> A.String . T.pack . show $ err
          Right res -> A.toJSON res
  let outputJSON = V.fromList . map showResult $ results

  -- Print results
  B.putStrLn . encode $ outputJSON 

execReq :: CLIRequest -> IO [Either StockError A.Value]
execReq (CLIRequest "price" batchSize (Just tickers)) = do
  let chunks = chunksOf batchSize tickers
  results <- fmap concat . sequence . map (Finnhub.quoteMany finnhubToken) $ chunks
  return $ map (bimap StockApiError A.toJSON) results
execReq (CLIRequest "sentiment" batchSize (Just tickers)) = do
  let chunks = chunksOf batchSize tickers
  results <- fmap concat . sequence . map (Finnhub.newsSentimentMany finnhubToken) $ chunks
  return $ map (bimap StockApiError A.toJSON) results
execReq req = return [Left . StockError . T.pack $ "unsupported operation. args: " ++ show req]

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
      <> help "Indicator to run" )
  <*> fmap Just stockTickers

stockTickers :: Parser [Text]
stockTickers = T.split (== ',') <$> argument str (metavar "TICKERS...")
