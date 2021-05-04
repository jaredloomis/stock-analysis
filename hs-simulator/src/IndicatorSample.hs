module IndicatorSample where

import RIO

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:))
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)

import qualified Data.Aeson as A

data IndicatorSample = IndicatorSample {
  sampleID            :: {-# UNPACK #-} !Text,
  sampleStockID       :: {-# UNPACK #-} !Text,
  sampleIndicatorID   :: {-# UNPACK #-} !Text,
  sampleFetchSourceID :: {-# UNPACK #-} !Text,
  sampleStartTime     :: {-# UNPACK #-} !UTCTime,
  sampleEndTime       :: {-# UNPACK #-} !UTCTime,
  sampleFetchTime     :: {-# UNPACK #-} !UTCTime,
  sampleValue         :: {-# UNPACK #-} !Double,
  sampleRaw           :: {-# UNPACK #-} !Text
} deriving (Show, Generic)

instance ToJSON IndicatorSample where
  toJSON sample = A.object [
      "sample_id"       .= sampleID sample,
      "stock_id"        .= sampleStockID sample,
      "indicator_id"    .= sampleIndicatorID sample,
      "fetch_source_id" .= sampleFetchSourceID sample,
      "start_time"      .= sampleStartTime sample,
      "end_time"        .= sampleEndTime sample,
      "fetch_time"      .= sampleFetchTime sample,
      "indicator_value" .= sampleValue sample,
      "indicator_raw"   .= sampleRaw sample
    ]
instance FromJSON IndicatorSample where
  parseJSON = A.withObject "IndicatorSample" $ \o -> IndicatorSample
        <$> o .: "sample_id"
        <*> o .: "stock_id"
        <*> o .: "indicator_id"
        <*> o .: "fetch_source_id"
        <*> o .: "start_time"
        <*> o .: "end_time"
        <*> o .: "fetch_time"
        <*> o .: "indicator_value"
        <*> o .: "indicator_raw"

instance Eq IndicatorSample where
  a == b = sampleValue a == sampleValue b

instance Ord IndicatorSample where
  compare a b = compare (sampleValue a) (sampleValue b)

diffSampleStartTime :: IndicatorSample -> IndicatorSample -> NominalDiffTime
diffSampleStartTime a b = diffUTCTime (sampleStartTime a) (sampleStartTime b)