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

import qualified Data.Text.IO as TIO

import FinnhubAPI (getQuote)
import Error (StockError(..))

main :: IO ()
main = do
  -- Parse command
  cmd <- execParser parserInfo

  -- Execute command
  res <- execCmd cmd

  -- Print results
  flip mapM_ res $ \case
    Left err  -> hPutStrLn stderr . show $ err
    Right res -> TIO.putStrLn res

execCmd :: StockCmd -> IO [Either StockError Text]
execCmd (QuoteCmd tickers) = do
  results <- mapM (getQuote finnhubToken) tickers
  return $ map (bimap StockApiError encode') results
 where
  encode' = toStrict . decodeUtf8With (\err val -> error $ err ++ show val) . encode

finnhubToken :: Text
finnhubToken = "bt0vb8n48v6qbloltab0"

simFinToken :: Text
simFinToken = "IY6lhcssERR4yPTO3QilYxqTpyIV7ylq"

data StockCmd =
  QuoteCmd { tickers :: [StockTicker] }

type StockTicker = Text

parserInfo :: ParserInfo StockCmd
parserInfo = info stockCmd (progDesc "Stock Data")

stockCmd :: Parser StockCmd
stockCmd = subparser
  (command "quote"
    (info quoteCmd (progDesc "Realtime Stock Quote")))

quoteCmd :: Parser StockCmd
quoteCmd = QuoteCmd <$> stockTickers

stockTickers :: Parser [StockTicker]
stockTickers = some (argument str (metavar "TICKERS..."))
