{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IndicatorConfig where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:))

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BS

loadIndicatorConfig :: FilePath -> IO IndicatorConfig
loadIndicatorConfig path = do
  txt <- TIO.readFile path
  return . fromJust $ A.decode . BS.fromStrict $ T.encodeUtf8 txt

newtype IndicatorConfig = IndicatorConfig {
  configMap :: M.Map T.Text DataSourceGroupSpec
} deriving (Eq, Show, Monoid, Semigroup, ToJSON, FromJSON)

data DataSourceGroupSpec = DataSourceGroupSpec {
  groupSources :: M.Map T.Text DataSourceSpec,
  groupAliases :: Maybe [T.Text]
} deriving (Eq, Show, Generic)

instance ToJSON DataSourceGroupSpec where
  toJSON (DataSourceGroupSpec sources aliases) = A.object $
    ("aliases" .= aliases) :
    M.elems (M.mapWithKey (\key value -> key .= value) sources)

instance FromJSON DataSourceGroupSpec where
  parseJSON (A.Object obj) = DataSourceGroupSpec <$> parseSources <*> obj .: "aliases"
   where
     parseSources :: A.Parser (M.Map T.Text DataSourceSpec)
     parseSources = sequence . fmap parseJSON . M.fromList . HM.toList $ obj

data DataSourceSpec = DataSourceSpec {
  command  :: Text,
  schedule :: [IndicatorTime]
} deriving (Eq, Show, Generic)

instance ToJSON DataSourceSpec
instance FromJSON DataSourceSpec

data IndicatorTime = IndicatorTime UTCTime | Now
  deriving (Eq, Show, Ord)

instance ToJSON IndicatorTime where
  toJSON Now = A.String "now"
  toJSON (IndicatorTime time) = toJSON time

instance FromJSON IndicatorTime where
  parseJSON node@(A.String txt)
    | T.toLower txt == "now" = pure Now
    | otherwise = IndicatorTime <$> parseJSON node

-- Indicators recognized by THIS package
recognizedIndicators :: [T.Text]
recognizedIndicators = ["price", "sentiment", "price-finnhub_local", "price-finnhub_api", "sentiment-finnhub_news"]
