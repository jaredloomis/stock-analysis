{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Sentiment where

import Data.Time (Day)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

data Sentiment a = Sentiment {
  sentimentTicker :: Text,
  -- | Period for which other sentiment applies
  sentimentPeriod :: (Day, Day),
  -- | Master sentiment, between 0 and 1
  sentimentMaster :: Double,
  -- | Volume of sentiment / excitement / "buzz", between 0 and 1
  sentimentBuzz :: Double,
  sentimentApiId :: Text,
  sentimentFetchDate :: Day,
  sentimentRaw :: a
} deriving (Show, Eq, Generic, ToJSON, FromJSON)