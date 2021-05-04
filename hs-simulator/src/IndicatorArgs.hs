module IndicatorArgs where

import RIO

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Time.Clock (UTCTime)
import Data.Hashable (Hashable(..))

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Time

data IndicatorArgs = IndicatorArgs !(IndicatorTime, IndicatorTime) !(HM.HashMap Text Any)
  deriving (Show, Eq, Generic, Hashable)

newtype Any = Any A.Value
  deriving (Show, Eq, Generic, Hashable)

instance ToJSON Any where
  toJSON (Any a) = toJSON a
instance FromJSON Any where
  parseJSON json = Any <$> parseJSON json
instance Ord Any where
  compare (Any a) (Any b) = compare (hashWithSalt 0 a) (hashWithSalt 0 b)
  {-
  compare (Any a) (Any b) = compareValues a b
   where
    compareValues (A.Object a) (A.Object b) = compare (HM.map typeOrdinality a) (HM.map typeOrdinality b)
    compareValues (A.Array a)  (A.Array b)  = compare (V.map typeOrdinality a) (V.map typeOrdinality b)
    compareValues (A.String a) (A.String b) = compare a b
    compareValues (A.Number a) (A.Number b) = compare a b
    compareValues (A.Bool a)   (A.Bool b)   = compare a b
    compareValues A.Null       A.Null       = EQ
    compareValues a            b            = compare (typeOrdinality a) (typeOrdinality b)

    typeOrdinality :: A.Value -> Word8
    typeOrdinality (A.Object _) = 5
    typeOrdinality (A.Array  _) = 4
    typeOrdinality (A.String _) = 3
    typeOrdinality (A.Number _) = 2
    typeOrdinality (A.Bool _)   = 1
    typeOrdinality A.Null       = 0
  }
  -}

lookupArg :: IndicatorArgs -> Text -> Maybe Any
lookupArg (IndicatorArgs _ args) name = name `HM.lookup` args

indicatorArgs :: (IndicatorTime, IndicatorTime) -> [(Text, Any)] -> IndicatorArgs
indicatorArgs timerange args = IndicatorArgs timerange (HM.fromList args)

arrayArg :: [A.Value] -> Any
arrayArg = Any . A.Array . V.fromList