{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module FinnhubAPI (getQuote) where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Quote (Quote(..))

type FinnhubAPI =
  "quote" :> QueryParam "token" Text :> QueryParam "symbol" Text :> Get '[JSON] FinnhubQuoteRaw

type FinnhubNumber = Float

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

finnhubAPI :: Proxy FinnhubAPI
finnhubAPI = Proxy

finnhubApiId :: Text
finnhubApiId = "finnhub"

quote :: Text -> Text -> ClientM FinnhubQuoteRaw
quote token symbol =
  client finnhubAPI (Just token) (Just symbol)

defaultClientEnv :: IO ClientEnv
defaultClientEnv = mkClientEnv
  <$> newManager tlsManagerSettings
  <*> parseBaseUrl "https://finnhub.io/api/v1"

getQuote :: Text -> Text -> IO (Either ClientError (Quote FinnhubQuoteRaw))
getQuote token symbol = do
  rawQuoteEither <- runClientM (quote token symbol) =<< defaultClientEnv
  return $ flip fmap rawQuoteEither $ \rawQuote ->
      Quote {
        quoteTicker = symbol,
        quoteTime   = epochToUTC (t rawQuote),
        quoteValue  = c rawQuote,
        quoteApiId  = finnhubApiId,
        quoteRaw    = rawQuote
      }

epochToUTC :: Integer -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral