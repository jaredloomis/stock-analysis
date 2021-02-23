{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module AlphaVantageAPI where

import GHC.Generics (Generic)
import Data.List (intersperse)
import Data.Proxy

import Data.Maybe
import Data.Aeson (ToJSON(..), FromJSON(..), (.:))
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Hashable
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Time
import Data.Time.LocalTime (LocalTime, localTimeToUTC)

import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A

import Quote (Quote(..))
import Sentiment (Sentiment(..))

import Debug.Trace (trace)

-- | https://www.alphavantage.co/documentation/
type AlphaVantageAPI =
      "query" :> FunctionParam :> SymbolParam :>
                 IntervalParam :> ApiKeyParam :>
                 Get '[JSON] AlphaVantageTimeSeries

type FunctionParam = QueryParamReq "function" Text
type SymbolParam   = QueryParamReq "symbol" Text
type IntervalParam = QueryParamReq "interval" Text
type ApiKeyParam   = QueryParamReq "apikey" Text
type QueryParamReq = QueryParam' '[Required, Strict]

type AlphaVantageNumber = Double
type AlphaVantageInt = Int

data AlphaVantageTimeSeries = AlphaVantageTimeSeries {
  metaData :: AlphaVantageTimeSeriesMetadata,
  timeseries :: HM.HashMap LocalTime AlphaVantageTimeSeriesQuote
} deriving (Eq, Show, Generic, ToJSON)

instance FromJSON AlphaVantageTimeSeries where
  parseJSON = A.withObject "quote" $ \o -> do
    metaData   <- o .: "Meta Data"
    timeseries <- o .: "Time Series (60min)" -- TODO only supports 60min interval
    return $ AlphaVantageTimeSeries metaData timeseries

data AlphaVantageTimeSeriesMetadata = AlphaVantageTimeSeriesMetadata {
  information :: Text,
  symbol :: Text,
  lastRefreshed :: LocalTime,
  interval :: Text,
  outputSize :: Text,
  timezone :: Text
} deriving (Eq, Show, Generic, ToJSON)

instance FromJSON AlphaVantageTimeSeriesMetadata where
  parseJSON = A.withObject "quote" $ \o ->
    AlphaVantageTimeSeriesMetadata
      <$> o .: "1. Information"
      <*> o .: "2. Symbol"
      <*> o .: "3. Last Refreshed"
      <*> o .: "4. Interval"
      <*> o .: "5. Output Size"
      <*> o .: "6. Time Zone"

data AlphaVantageTimeSeriesQuote = AlphaVantageTimeSeriesQuote {
  open   :: Maybe AlphaVantageNumber,
  high   :: Maybe AlphaVantageNumber,
  low    :: Maybe AlphaVantageNumber,
  close  :: Maybe AlphaVantageNumber,
  volume :: Maybe AlphaVantageInt
} deriving (Eq, Show, Generic, ToJSON)

instance FromJSON AlphaVantageTimeSeriesQuote where
  parseJSON = A.withObject "quote" $ \o ->
    let readDouble = read :: String -> Double
        readInt    = read :: String -> Int
    in AlphaVantageTimeSeriesQuote
      <$> fmap (fmap readDouble) (o .: "1. open")
      <*> fmap (fmap readDouble) (o .: "2. high")
      <*> fmap (fmap readDouble) (o .: "3. low")
      <*> fmap (fmap readDouble) (o .: "4. close")
      <*> fmap (fmap readInt)    (o .: "5. volume")

{-
data AlphaVantageInterval = Every1Min | Every5Min | Every15Min | Every30Min | Every60Min
  deriving (Eq, Ord)

instance Show AlphaVantageInterval where
  show Min1  = "1min"
  show Min5  = "5min"
  show Min15 = "15min"
  show Min30 = "30min"
  show Min60 = "60min"
instance ToJSON 
-}

instance Hashable LocalTime where
  hashWithSalt salt time = salt + hashWithSalt salt (show time)

alphaVantageAPI :: Proxy AlphaVantageAPI
alphaVantageAPI = Proxy

dataSourceID :: Text
dataSourceID = "alpha_vantage_api"

queryC :: Text -> Text -> Text -> Text -> ClientM AlphaVantageTimeSeries
queryC = client alphaVantageAPI

defaultClientEnv :: IO ClientEnv
defaultClientEnv = mkClientEnv
  <$> newManager tlsManagerSettings
  <*> parseBaseUrl "https://www.alphavantage.co"

queryRaw :: Text -> Text -> Text -> Text -> IO (Either ClientError [Quote AlphaVantageTimeSeriesQuote])
queryRaw function symbol interval apiKey = do
  rawQuoteEither <- runClientM (queryC function symbol interval apiKey) =<< defaultClientEnv
  pure $ flip fmap rawQuoteEither $ \(AlphaVantageTimeSeries metaData timeseries) ->
        HM.elems $
        flip HM.mapWithKey timeseries $ \time quote ->
            Quote {
              quoteTicker = symbol,
              quoteTime   = localTimeToUTC (parseTimeZone (timezone metaData)) time,
              quoteValue  = fromMaybe (-1) $ close quote <|> low quote <|> high quote <|> open quote,
              quoteApiId  = dataSourceID,
              quoteRaw    = quote
            }

quote :: Text -> Text -> IO (Either ClientError [Quote AlphaVantageTimeSeriesQuote])
quote apiKey ticker = queryRaw "TIME_SERIES_INTRADAY" ticker "60min" apiKey

quoteMany :: Text -> [Text] -> IO [Either ClientError (Quote AlphaVantageTimeSeriesQuote)]
quoteMany apiKey = fmap (>>= sequence) . mapM (quote apiKey)

parseTimeZone :: Text -> TimeZone
parseTimeZone = fromMaybe utc . parseTimeM True defaultTimeLocale "%Z" . T.unpack -- TODO the timezone parsing isn't actually working
