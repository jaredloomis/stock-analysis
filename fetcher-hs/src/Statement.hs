{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Statement where

import GHC.Generics (Generic)

import Data.Text  (Text)
import Data.Time (UTCTime, Day)

import Data.Aeson

data FundamentalsStatement a = FundamentalsStatement {
  -- Basic info
  stmtTicker :: Text,
  stmtPeriod :: (Day, Day),
  -- Fundamentals
  stmtNetProfit :: Maybe Float,
  stmtShareholdersEquity :: Maybe Float,
  -- Metadata regarding the data source
  stmtPublishDate :: Day,
  -- | The date this FundamentalsStatement was created.
  stmtFetchDate :: Day,
  stmtSource :: Text,
  stmtApiId :: Text,
  stmtRaw :: a
} deriving (Eq, Show, Generic, ToJSON, FromJSON)
