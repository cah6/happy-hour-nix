{-# LANGUAGE LambdaCase, DeriveGeneric, OverloadedStrings, DataKinds, RecordWildCards, TypeOperators, TemplateHaskell #-}
module Common.Api where

import qualified Data.Attoparsec.Text as AP
import           Data.Attoparsec.Text (decimal, char)
import qualified Money
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.Text (Text, pack, breakOn)
import Data.Time.Exts.Base hiding (pack)
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (TimeOfDay(..))
import System.Directory (listDirectory)

import Common.Helper (attoToAeson)
import GHC.Generics (Generic)

type USD = Money.Discrete "USD" "cent"

newtype TimeRange = TimeRange (TimeOfDay, TimeOfDay)
  deriving (Generic, Show)

instance FromJSON TimeRange where
  parseJSON = attoToAeson parseTimeRange

loadHHs :: IO (Either String [HappyHour])
loadHHs = do
  let baseDir = "resources/data/"
  filenames <- listDirectory baseDir
  bytestrings <- traverse (B.readFile . (<>) baseDir) filenames
  return $ traverse eitherDecode bytestrings

parseTimeRange :: AP.Parser TimeRange
parseTimeRange = do
  startH <- decimal
  char ':'
  startM <- decimal
  char '-'
  endH <- decimal
  char ':'
  endM <- decimal
  return $ TimeRange (TimeOfDay startH startM 0, TimeOfDay endH endM 0)

data HappyHour = HappyHour
  { _city :: Text
  , _restaurant :: Text
  , _schedule :: [Schedule]
  , _link :: Text
  } deriving (Generic, Show)

instance FromJSON HappyHour where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Schedule = Schedule
  { _days :: [DayOfWeek Gregorian]
  , _time :: TimeRange
  , _scheduleDescription :: Text
  } deriving (Generic, Show)

instance FromJSON Schedule where
  parseJSON = withObject "schedule" $ \o -> do
    _days                 <- map read <$> o .: "days" -- read for String -> TimeOfDay
    _time                 <- o .: "time" >>= parseJSON
    _scheduleDescription  <- o .: "scheduleDescription"
    return Schedule{..}

data MenuItem = MenuItem
  { _id :: Int
  , _itemDescription :: Text
  , _price :: USD
  } deriving (Generic, Show)

instance FromJSON MenuItem where
  parseJSON = withObject "menuItem" $ \o -> do
    _id <- o .: "id"
    _itemDescription  <- o .: "itemDescription"
    _price <- fromInteger <$> o .: "price"
    return MenuItem{..}

mkUSD :: Integer -> USD
mkUSD = fromInteger

ppUSD :: USD -> Text
ppUSD usd = "$" <> (pack . show) dollar <> "." <> (pack . show) cents
  where (dollar, cents) = divMod (toInteger usd) 100

getMenu :: IO B.ByteString
getMenu = B.readFile "resources/data/menu_item.json"

-- goal for now
jnkMenu :: IO B.ByteString
jnkMenu = B.readFile "resources/data/johnny_noodle_king.json"

parseSchedule :: B.ByteString -> Either String Schedule
parseSchedule bs = eitherDecode bs

