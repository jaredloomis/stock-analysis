{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module FinnhubLocal (dataSourceID, quoteMany, quote, FinnhubQuoteRaw) where

import GHC.Generics (Generic)
import Data.List (intersperse, find)
import Data.Proxy
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2, (<|>))

import Data.Aeson
import Data.Text (Text)
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Time (UTCTime(..), Day, getCurrentTime, utctDay, toGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.FilePath.Posix ((</>))

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Csv as C
import qualified Data.Set as S

import Quote (Quote(..))
import Sentiment (Sentiment(..))
import Error

type FinnhubNumber = Double

dataSourceID = "finnhub_local"

data FinnhubQuoteRaw = FinnhubQuoteRaw {
  finnhubID :: Text,
  symbol :: Text,
  klass :: Text,
  open :: Maybe FinnhubNumber,
  high :: Maybe FinnhubNumber,
  low :: Maybe FinnhubNumber,
  close :: Maybe FinnhubNumber,
  volume :: Int,
  div :: Text,
  adjustment :: Text,
  bid :: Maybe FinnhubNumber,
  ask :: Maybe FinnhubNumber
} deriving (Eq, Show, Generic)

instance ToJSON FinnhubQuoteRaw where
  toJSON (FinnhubQuoteRaw finnhubID symbol klass open high low close volume div adjustment bid ask) = object [
      "finnhub_id" .= finnhubID,
      "symbol" .= symbol,
      "class" .= klass,
      "open" .= open,
      "high" .= high,
      "low" .= low,
      "close" .= close,
      "volume" .= volume,
      "div" .= div,
      "adjustment" .= adjustment,
      "bid" .= bid,
      "ask" .= ask
    ]

instance FromJSON FinnhubQuoteRaw where
  parseJSON = withObject "quote" $ \o -> do
    finnhubID  <- o .: "finnhub_id"
    symbol     <- o .: "symbol"
    klass      <- o .: "class"
    open       <- o .: "open"
    high       <- o .: "high"
    low        <- o .: "low"
    close      <- o .: "close"
    volume     <- o .: "volume"
    div        <- o .: "div"
    adjustment <- o .: "adjustment"
    bid        <- o .: "bid"
    ask        <- o .: "ask"
    return $ FinnhubQuoteRaw finnhubID symbol klass open high low close volume div adjustment bid ask

select :: C.FromField a => BI.ByteString -> C.NamedRecord -> C.Parser a
select = flip C.lookup

instance C.FromNamedRecord FinnhubQuoteRaw where
  parseNamedRecord r = FinnhubQuoteRaw
    <$> select "finnhub_id" r
    <*> select "symbol" r
    <*> select "class" r
    <*> select "open" r
    <*> select "high" r
    <*> select "low" r
    <*> select "close" r
    <*> select "volume" r
    <*> select "div" r
    <*> select "adjustment" r
    <*> select "bid" r
    <*> select "ask" r

-- | 'Greedy' indicates that this function *returns unrelated quotes along with the requested quote*.
--   The line of thinking is: if we're already opening the file and parsing it, we might as well return the
--   parsed quotes.
quoteGreedy :: Day -> Text -> IO (Either StockError [Quote FinnhubQuoteRaw])
quoteGreedy day symbol = do
  let file = finnhubDataPrefix </> fileNameForDay day
  csv <- B.readFile file
  let rawQuoteEither = C.decodeByName csv
  return $ bimap LocalIOError (map normalizeQuote . V.toList . snd) rawQuoteEither
 where
   normalizeQuote rawQuote =
      Quote {
        quoteTicker = FinnhubLocal.symbol rawQuote,
        quoteTime   = dayToUTC day,
        quoteValue  = fromMaybe (-1) $ close rawQuote <|> open rawQuote <|> high rawQuote <|> low rawQuote <|> bid rawQuote <|> ask rawQuote,
        quoteApiId  = dataSourceID,
        quoteRaw    = rawQuote
      }

   finnhubDataPrefix :: FilePath
   finnhubDataPrefix = "data" </> "finnhub" </> "stock-eod-prices"

   fileNameForDay :: Day -> FilePath
   fileNameForDay day =
     let (year, month, dayNum) = toGregorian day
     in show year ++ showAtLeastTwoDigits month ++ showAtLeastTwoDigits dayNum ++ ".csv"

   showAtLeastTwoDigits num
     | num < 10 && num >= 0 = '0' : show num
     | otherwise            = show num

   dayToUTC :: Day -> UTCTime
   dayToUTC day = UTCTime day 0

quote :: Day -> Text -> IO (Either StockError (Quote FinnhubQuoteRaw, [Quote FinnhubQuoteRaw]))
quote day symbol = do
  quotes <- quoteGreedy day symbol
  return $ liftA2 (,) (collapseError . fmap (find correctSymbol) $ quotes) quotes
 where
  correctSymbol quote = FinnhubLocal.symbol (quoteRaw quote) == symbol

  collapseError :: Either StockError (Maybe (Quote FinnhubQuoteRaw)) -> Either StockError (Quote FinnhubQuoteRaw)
  collapseError = fromMaybe (Left noMatchError) . sequence

  noMatchError = StockError "Didn't find any quotes in csv matching specified symbol."

quoteMany :: Day -> [Text] -> IO (Either StockError [Quote FinnhubQuoteRaw])
quoteMany day symbols = do
  let file = finnhubDataPrefix </> fileNameForDay day
  csv <- B.readFile file
  let rawQuoteEither = C.decodeByName csv
  return $ bimap LocalIOError (map normalizeQuote . filter isRelevant . V.toList . snd) rawQuoteEither
 where
   isRelevant rawQuote = FinnhubLocal.symbol rawQuote `S.member` symbolSet
   symbolSet = S.fromList symbols

   normalizeQuote rawQuote =
      Quote {
        quoteTicker = FinnhubLocal.symbol rawQuote,
        quoteTime   = dayToUTC day,
        quoteValue  = fromMaybe (-1) $ close rawQuote <|> open rawQuote <|> high rawQuote <|> low rawQuote <|> bid rawQuote <|> ask rawQuote,
        quoteApiId  = dataSourceID,
        quoteRaw    = rawQuote
      }

   finnhubDataPrefix :: FilePath
   finnhubDataPrefix = "data" </> "finnhub" </> "stock-eod-prices"

   fileNameForDay :: Day -> FilePath
   fileNameForDay day =
     let (year, month, dayNum) = toGregorian day
     in show year ++ showAtLeastTwoDigits month ++ showAtLeastTwoDigits dayNum ++ ".csv"

   showAtLeastTwoDigits num
     | num < 10 && num >= 0 = '0' : show num
     | otherwise            = show num

   dayToUTC :: Day -> UTCTime
   dayToUTC day = UTCTime day 0
