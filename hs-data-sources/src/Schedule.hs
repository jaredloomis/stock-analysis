{-# LANGUAGE OverloadedStrings #-}
module Schedule where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Foldable (Foldable)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=))

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Map as M

import IndicatorConfig

availableSources :: IndicatorTime -> DataSourceGroupSpec -> [(DataSourceSpec, T.Text)]
availableSources timestamp = M.foldrWithKey filterAccum [] . groupSources
 where
  filterAccum name source acc =
    if source `isAvailableAt` timestamp
      then (source, name) : acc
      else acc

isAvailableAt :: DataSourceSpec -> IndicatorTime -> Bool
isAvailableAt spec timestamp =
  let [start, end] = schedule spec
  in start <= timestamp && timestamp <= end
