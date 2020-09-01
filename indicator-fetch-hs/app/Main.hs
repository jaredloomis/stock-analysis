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

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified FinnhubAPI as Finnhub
import Error (StockError(..))

main :: IO ()
main = do
  -- Parse request
  req <- execParser parserInfo

  -- Execute request
  res <- execReq req

  -- Print results
  flip mapM_ res $ \case
    Left err  -> hPutStrLn stderr . show $ err
    Right res -> TIO.putStrLn res

execReq :: CLIRequest -> IO [Either StockError Text]
execReq (CLIRequest "price" (Just tickers)) = do
  results <- mapM (Finnhub.quote finnhubToken) tickers
  return $ map (bimap StockApiError toJSON) results
execReq (CLIRequest "sentiment" (Just tickers)) = do
  results <- mapM (Finnhub.newsSentiment finnhubToken) tickers
  return $ map (bimap StockApiError toJSON) results
execReq req = return [Left . StockError . T.pack $ "unsupported operation. args: " ++ show req]

toJSON :: ToJSON a => a -> Text
toJSON = toStrict . decodeUtf8With (\err val -> error $ err ++ show val) . encode

--
-- Config
--

finnhubToken :: Text
finnhubToken = "bt0vb8n48v6qbloltab0"

simFinToken :: Text
simFinToken = "IY6lhcssERR4yPTO3QilYxqTpyIV7ylq"

--
-- Arg parsing
--

data CLIRequest = CLIRequest {
  reqIndicator :: Text,
  reqTickers   :: Maybe [Text]
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
  <*> fmap Just stockTickers

stockTickers :: Parser [Text]
stockTickers = many (argument str (metavar "TICKERS..."))
