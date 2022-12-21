{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassType where

import GHC.Generics
import Data.Aeson.Types (FromJSON,ToJSON,typeMismatch,prependFailure)
import Data.Aeson
import qualified Data.Aeson.KeyMap as D

data RoundInfo = RoundInfo {
    id :: Int 
    ,calendarId :: Int
    ,name :: String
    }
        deriving (Generic, Show)

data RoundMap = RoundMap [RoundInfo]
        deriving (Generic, Show)

data Lesson = Lesson {
    teachClassId :: Int 
    ,teachClassCode :: String 
    ,courseCode :: String 
    }
        deriving (Generic, Show)

data SelectPkg = SelectPkg {
    roundId :: Int 
    ,elecClassList :: [Lesson]
    ,withdrawClassList :: [Lesson]
    }
        deriving (Generic, Show)


instance ToJSON RoundInfo where 
instance ToJSON Lesson where 
instance ToJSON SelectPkg where 
instance FromJSON RoundMap where 
    parseJSON (Object v) = RoundMap <$> v .: "data"
    parseJSON invalid = prependFailure "parsing Coord failed, " (typeMismatch "Object" invalid)


instance FromJSON RoundInfo where 
instance FromJSON SelectPkg where 
instance FromJSON Lesson where 
