{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module FinnhubLocal where

import GHC.Generics (Generic)
import Data.List (intersperse, find)
import Data.Proxy
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)

import Data.Aeson
import Data.Text (Text)
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Time (UTCTime(..), Day, getCurrentTime, utctDay, toGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Csv as C

import Quote (Quote(..))
import Sentiment (Sentiment(..))
import Error

type FinnhubNumber = Double

finnhubLocalApiId = "finnhub-local-files"

data FinnhubQuoteRaw = FinnhubQuoteRaw {
  finnhubID :: Text,
  symbol :: Text,
  klass :: Text,
  open :: FinnhubNumber,
  high :: FinnhubNumber,
  low :: FinnhubNumber,
  close :: FinnhubNumber,
  volume :: Int,
  div :: Text,
  adjustment :: Text,
  bid :: FinnhubNumber,
  ask :: FinnhubNumber
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

quoteMany :: Day -> Text -> IO (Either StockError [Quote FinnhubQuoteRaw])
quoteMany day symbol = do
  let file = filePathForDay day
  csv <- B.readFile file
  let rawQuoteEither = C.decodeByName csv
  return $ bimap LocalIOError (map normalizeQuote . V.toList . snd) rawQuoteEither
 where
   normalizeQuote rawQuote =
      Quote {
        quoteTicker = symbol,
        quoteTime   = dayToUTC day,
        quoteValue  = close rawQuote,
        quoteApiId  = finnhubLocalApiId,
        quoteRaw    = rawQuote
      }

   filePathForDay :: Day -> FilePath
   filePathForDay day =
     let (year, month, dayNum) = toGregorian day
     in show year ++ show month ++ show dayNum ++ ".csv"

   dayToUTC :: Day -> UTCTime
   dayToUTC day = UTCTime day 0

quote :: Day -> Text -> IO (Either StockError (Quote FinnhubQuoteRaw, [Quote FinnhubQuoteRaw]))
quote day symbol = do
  quotes <- quoteMany day symbol
  return $ liftA2 (,) (collapseError . fmap (find correctSymbol) $ quotes) quotes
 where
  correctSymbol quote = FinnhubLocal.symbol (quoteRaw quote) == symbol

  collapseError :: Either StockError (Maybe (Quote FinnhubQuoteRaw)) -> Either StockError (Quote FinnhubQuoteRaw)
  collapseError = fromMaybe (Left noMatchError) . sequence

  noMatchError = StockError "Didn't find any quotes in csv matching specified symbol."
