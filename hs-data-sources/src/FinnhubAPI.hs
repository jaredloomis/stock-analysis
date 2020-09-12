{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module FinnhubAPI (quote, newsSentiment, quoteMany, newsSentimentMany) where

import GHC.Generics (Generic)
import Data.List (intersperse)
import Data.Proxy

import Data.Aeson
import Data.Text (Text)
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Time (UTCTime, Day, getCurrentTime, utctDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Quote (Quote(..))
import Sentiment (Sentiment(..))

type FinnhubAPI =
      "quote"          :> TokenParam :> SymbolParam :> Get '[JSON] FinnhubQuoteRaw
 :<|> "news-sentiment" :> TokenParam :> SymbolParam :> Get '[JSON] FinnhubNewsSentimentRes

type TokenParam    = QueryParamReq "token" Text
type SymbolParam   = QueryParamReq "symbol" Text
type QueryParamReq = QueryParam' '[Required, Strict]

type FinnhubNumber = Double

data FinnhubQuoteRaw = FinnhubQuoteRaw {
  -- | Current Value
  c :: FinnhubNumber,
  -- | High of the day
  h :: FinnhubNumber,
  -- | Low of the day
  l :: FinnhubNumber,
  -- | Open Price
  o :: FinnhubNumber,
  -- | Previous closing price
  pc :: FinnhubNumber,
  -- | UNIX timestamp
  t :: Integer 
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FinnhubNewsSentimentRes = FinnhubNewsSentimentRes {
  buzz :: FinnhubNewsSentimentBuzz,
  sentiment :: FinnhubNewsSentimentSent,
  companyNewsScore :: FinnhubNumber,
  sectorAverageBullishPercent :: FinnhubNumber,
  sectorAverageNewsScore :: FinnhubNumber,
  symbol :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FinnhubNewsSentimentBuzz = FinnhubNewsSentimentBuzz {
  articlesInLastWeek :: Integer,
  buzzScore :: FinnhubNumber,
  weeklyAverage :: FinnhubNumber
} deriving (Eq, Show, Generic)

instance ToJSON FinnhubNewsSentimentBuzz where
  toJSON (FinnhubNewsSentimentBuzz articlesInLastWeek buzzScore weeklyAverage) = object [
      "articlesInLastWeek" .= articlesInLastWeek,
      "buzz" .= buzzScore,
      "weeklyAverage" .= weeklyAverage
    ]

instance FromJSON FinnhubNewsSentimentBuzz where
  parseJSON = withObject "statement" $ \o -> do
    articlesInLastWeek <- o .: "articlesInLastWeek"
    buzzScore          <- o .: "buzz"
    weeklyAverage      <- o .: "weeklyAverage"
    return $ FinnhubNewsSentimentBuzz articlesInLastWeek buzzScore weeklyAverage

data FinnhubNewsSentimentSent = FinnhubNewsSentimentSent {
  bearishPercent :: FinnhubNumber,
  bullishPercent :: FinnhubNumber
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

finnhubAPI :: Proxy FinnhubAPI
finnhubAPI = Proxy

finnhubApiId :: Text
finnhubApiId = "finnhub"

getQuote :: Text -> Text -> ClientM FinnhubQuoteRaw
getNewsSentiment :: Text -> Text -> ClientM FinnhubNewsSentimentRes
getQuote :<|> getNewsSentiment = client finnhubAPI

defaultClientEnv :: IO ClientEnv
defaultClientEnv = mkClientEnv
  <$> newManager tlsManagerSettings
  <*> parseBaseUrl "https://finnhub.io/api/v1"

quote :: Text -> Text -> IO (Either ClientError (Quote FinnhubQuoteRaw))
quote token symbol = do
  rawQuoteEither <- runClientM (getQuote token symbol) =<< defaultClientEnv
  return $ flip fmap rawQuoteEither $ \rawQuote ->
      Quote {
        quoteTicker = symbol,
        quoteTime   = epochToUTC (t rawQuote),
        quoteValue  = c rawQuote,
        quoteApiId  = finnhubApiId,
        quoteRaw    = rawQuote
      }

quoteMany :: Text -> [Text] -> IO [Either ClientError (Quote FinnhubQuoteRaw)]
quoteMany token = mapM (quote token)

newsSentiment :: Text -> Text -> IO (Either ClientError (Sentiment FinnhubNewsSentimentRes))
newsSentiment token symbol = do
  rawQuoteEither <- runClientM (getNewsSentiment token symbol) =<< defaultClientEnv
  today <- getCurrentDay
  return $ flip fmap rawQuoteEither $ \rawSentiment ->
    Sentiment {
      sentimentTicker = symbol,
      sentimentPeriod = (today, today),
      sentimentMaster = finnhubMasterSentiment rawSentiment,
      sentimentBuzz = finnhubBuzz rawSentiment,
      sentimentApiId = finnhubApiId,
      sentimentFetchDate = today,
      sentimentRaw = rawSentiment
    }

newsSentimentMany :: Text -> [Text] -> IO [Either ClientError (Sentiment FinnhubNewsSentimentRes)]
newsSentimentMany token = mapM (newsSentiment token)

finnhubMasterSentiment :: FinnhubNewsSentimentRes -> FinnhubNumber
finnhubMasterSentiment = bullishPercent . sentiment

finnhubBuzz :: FinnhubNewsSentimentRes -> FinnhubNumber
finnhubBuzz = buzzScore . buzz

epochToUTC :: Integer -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral

getCurrentDay :: IO Day
getCurrentDay = utctDay <$> getCurrentTime