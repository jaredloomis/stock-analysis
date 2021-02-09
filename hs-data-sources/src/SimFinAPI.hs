{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Their HTTP API doesn't seem to be functional. Should've checked first :/
module SimFinAPI (getCompanyStatements) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON, encode, (.:), (.=), withObject, object)
import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Time (Day, fromGregorian)
import Data.Vector ((!?))

import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Statement (FundamentalsStatement(..))

type SimFinAPI =
  "companies" :> "statements" :> QueryParam "apiKey" Text :> QueryParam "ticker" Text :> Get '[JSON] SimFinStatement

type FinnhubNumber = Float

data SimFinStatement = SimFinStatement {
  sfstmtFound    :: Bool,
  sfstmtCurrency :: Text,
  sfstmtColumns  :: V.Vector Text,
  sfstmtData     :: V.Vector A.Value
} deriving (Eq, Show)

instance ToJSON SimFinStatement where
  toJSON (SimFinStatement found currency columns dat) = object [
      "found" .= found,
      "currency" .= currency,
      "columns" .= columns,
      "data" .= dat
    ]

instance FromJSON SimFinStatement where
  parseJSON = withObject "statement" $ \o -> do
    found    <- o .: "found"
    currency <- o .: "currency"
    columns  <- o .: "columns"
    dat      <- o .: "data"
    return $ SimFinStatement found currency columns dat

simFinAPI :: Proxy SimFinAPI
simFinAPI = Proxy

simFinApiId :: Text
simFinApiId = "simfin_api"

companyStatements :: Text -> Text -> ClientM SimFinStatement
companyStatements apiKey ticker =
  client simFinAPI (Just apiKey) (Just ticker)

defaultClientEnv :: IO ClientEnv
defaultClientEnv = mkClientEnv
  <$> newManager tlsManagerSettings
  <*> parseBaseUrl "https://simfin.com/api/v2"

getCompanyStatements :: Text -> Text -> IO (Either ClientError (FundamentalsStatement SimFinStatement))
getCompanyStatements apiKey ticker = do
  rawStmtEither <- runClientM (companyStatements apiKey ticker) =<< defaultClientEnv
  return $ flip fmap rawStmtEither $ \rawStmt -> (undefined :: FundamentalsStatement SimFinStatement)
    {-
      FundamentalsStatement {
        -- Basic info
        stmtTicker = ticker,
        stmtPeriod :: (UTCTime, UTCTime),
        -- Fundamentals
        stmtNetProfit :: Maybe Float,
        stmtShareholdersEquity :: Maybe Float,
        -- Metadata regarding the data source
        stmtPublishDate :: UTCTime,
        stmtSource :: Text,
        stmtApiId :: Text,
        stmtRaw :: a
      }-}

{-
standardizeStmt :: Text -> SimFinStatement -> Maybe (FundamentalsStatement SimFinStatement)
standardizeStmt ticker stmt@(SimFinStatement _ _ columns stmtData) =
  let (Just netProfit) = findStmtValueByLowerKey "net income" stmt
      (Just fiscalPeriod) = findStmtValueByLowerKey "shareholder's equity"
  in do
    fiscalPeriod <- findFiscalPeriod stmt
    fiscalYear   <- findFiscalYear stmt

    return $ FundamentalsStatement {
        -- Basic info
        stmtTicker = ticker,
        stmtPeriod = fiscalPeriodToDayRange fiscalPeriod fiscalYear,
        -- Fundamentals
        stmtNetProfit = findNetProfit stmt,
        stmtShareholdersEquity :: Maybe Float,
        -- Metadata regarding the data source
        stmtPublishDate :: UTCTime,
        stmtSource :: Text,
        stmtApiId :: Text,
        stmtRaw :: a
      }
-}

fiscalPeriodToDayRange :: Text -> Int -> (Day, Day)
fiscalPeriodToDayRange fiscalPeriod year =
  let (startMonth, endMonth) = case fiscalPeriod of
        "Q1" -> (1, 3)
        "Q2" -> (4, 6)
        "Q3" -> (7, 9)
        "Q4" -> (9, 12)
  in (
      fromGregorian (toInteger year) startMonth 1,
      fromGregorian (toInteger year) endMonth 32
    )

findNetProfit :: SimFinStatement -> Maybe Float
findNetProfit stmt =
  findStmtValueByLowerKey "Net Income" stmt >>= \case
    A.String txt -> Just . fst . fromRight $ (T.signed T.rational) txt
    _            -> Nothing
 where
  fromRight (Right a) = a

findShareholdersEquity :: SimFinStatement -> Maybe Float
findShareholdersEquity stmt =
  findStmtValueByLowerKey "Shareholder's Equity" stmt >>= \case
    A.String txt -> Just . fst . fromRight $ (T.signed T.rational) txt
    _            -> Nothing
 where
  fromRight (Right a) = a

findFiscalPeriod :: SimFinStatement -> Maybe Text
findFiscalPeriod stmt =
  findStmtValueByLowerKey "Fiscal Period" stmt >>= \case
    A.String txt -> Just txt
    _            -> Nothing

findFiscalYear :: SimFinStatement -> Maybe Text
findFiscalYear stmt =
  findStmtValueByLowerKey "Fiscal Year" stmt >>= \case
    A.String txt -> Just txt
    _            -> Nothing

findStmtValueByLowerKey :: Text -> SimFinStatement -> Maybe A.Value
findStmtValueByLowerKey key = findStmtValue (\txt -> T.toLower txt == key)

findStmtValue :: (Text -> Bool) -> SimFinStatement -> Maybe A.Value
findStmtValue columnCondition (SimFinStatement _ _ columns stmtData) =
  (stmtData !?) =<< V.findIndex columnCondition columns
