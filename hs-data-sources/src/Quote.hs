{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
module Quote (Quote(..), AnyQuote) where

import GHC.Generics (Generic)

import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Aeson

-- | Stores a quote for a stock at a specific time
data Quote a = Quote {
  quoteTicker :: !Text,
  quoteTime :: !UTCTime,
  quoteValue :: !Double,
  quoteApiId :: !Text,
  quoteRaw :: !a
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AnyQuote = forall a. AnyQuote (Quote a)
