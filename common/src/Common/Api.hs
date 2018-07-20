{-# LANGUAGE LambdaCase, DeriveGeneric, OverloadedStrings, DataKinds, RecordWildCards, TypeOperators, TemplateHaskell #-}
module Common.Api where

import qualified Money as Money
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Text as AP
import Data.Attoparsec.Text (satisfy, scientific)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Time.Exts.Base hiding (pack)
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (TimeOfDay(..))

import GHC.Generics (Generic)
import Common.Helper (attoAeson)

type USD = Money.Discrete "USD" "cent"

data HappyHour = HappyHour 
  { _city :: Text
  , _restaurant :: Text
  , _schedule :: [Schedule]
  } deriving (Generic, Show)

data Schedule = Schedule
  { _days :: [DayOfWeek Gregorian]
  , _time :: (TimeOfDay, TimeOfDay)
  , _scheduleDescription :: Text
  , _details :: [MenuItem]
  } deriving (Generic, Show)

instance FromJSON Schedule where
  parseJSON = withObject "schedule" $ \o -> do
    _days <- (fmap . fmap) read (o .: "days")
    _time  <- attoAeson mkTimeRange (o .: "time")
    _scheduleDescription  <- o .: "scheduleDescription"
    _details <- return []  
    return Schedule{..}

mkTimeRange :: AP.Parser (TimeOfDay, TimeOfDay)
mkTimeRange = do 
  startH <- coefficient <$> scientific
  satisfy ":"
  startM <- coefficient <$> scientific
  satisfy "-"
  endH <- coefficient <$> scientific
  satisfy ":"
  endM <- coefficient <$> scientific
  return (TimeOfDay startH startM 0, TimeOfDay endH endM, 0) 

takeTillColon :: AP.Parser Text
takeTillColon = takeTill2 (\c -> c == ':')

takeTillDash :: AP.Parser Text
takeTillDash = takeTill2 (\c -> c == '-')

takeTill2 :: (Char -> Bool) -> AP.Parser Text
takeTill2 f = do
  val <- AP.takeTill f
  _ <- AP.satisfy f
  return val

data MenuItem = MenuItem
  { _id :: Int
  , _itemDescription :: Text
  , _price :: USD
  } deriving (Generic, Show)

instance FromJSON MenuItem where
  parseJSON = withObject "menuItem" $ \o -> do
    _id <- o .: "id"
    _itemDescription  <- o .: "itemDescription"
    _price <- fromInteger <$> o.: "price"  
    return MenuItem{..}

mkUSD :: Integer -> USD
mkUSD = fromInteger

ppUSD :: USD -> Text
ppUSD usd = "$" <> (pack . show) dollar <> "." <> (pack . show) cents
    where 
  (dollar, cents) = divMod (toInteger usd) 100

getMenu :: IO B.ByteString 
getMenu = B.readFile "resources/data/menu_item.json"

-- goal for now
jnkMenu :: IO (B.ByteString)
jnkMenu = B.readFile "resources/data/johnny_noodle_king.json"
