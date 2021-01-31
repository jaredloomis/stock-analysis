module Time where

import RIO
import RIO.Time (getCurrentTime, addUTCTime)

import Data.Text (pack, unpack)
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Hashable (Hashable(..))

import qualified Data.Aeson as A

data IndicatorTime = IndicatorTime {-# UNPACK #-} !UTCTime | Now
  deriving (Show, Eq, Ord, Generic)

instance Hashable IndicatorTime where
  hashWithSalt salt Now = salt + 171
  hashWithSalt salt (IndicatorTime t) = salt + (floor $ (realToFrac (utcTimeToPOSIXSeconds t) :: Double) * 1000)
instance ToJSON IndicatorTime where
  toJSON Now               = toJSON ("now" :: String)
  toJSON (IndicatorTime t) = toJSON t
instance FromJSON IndicatorTime where
  parseJSON (A.String "now") = pure Now
  parseJSON time             = IndicatorTime <$> parseJSON time

addTime :: IndicatorTime -> NominalDiffTime -> IO IndicatorTime
addTime time diff = mapUTC (addUTCTime diff) time

mapUTC :: (UTCTime -> UTCTime) -> IndicatorTime -> IO IndicatorTime
mapUTC f time = IndicatorTime . f <$> toUTC time

mapMUTC :: (UTCTime -> IO UTCTime) -> IndicatorTime -> IO IndicatorTime
mapMUTC f time = IndicatorTime <$> (f =<< toUTC time)

toUTC :: IndicatorTime -> IO UTCTime
toUTC Now                  = getCurrentTime
toUTC (IndicatorTime time) = pure time

showTime :: ISO8601 t => t -> Text
showTime = pack . iso8601Show

parseTime :: Text -> Maybe UTCTime
parseTime = formatParseM format . unpack

--format :: Format UTCTime
format = utcTimeFormat (calendarFormat ExtendedFormat) (timeOfDayFormat ExtendedFormat)
