{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Toggl.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Fixed (Centi)
import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime)
import GHC.Generics (Generic)

type Money = Centi

data TimeEntry = TimeEntry
    { teId :: Int

    , teUid :: Int
    , teUser :: Text

    , teTid :: Maybe Int
    , teTask :: Maybe Text

    , tePid :: Int
    , teProject :: Text

    -- Not documented
    , teProjectColor :: Text
    , teProjectHexColor :: Text

    , teClient :: Text

    , teDescription :: Text

    , teStart :: UTCTime
    , teEnd :: UTCTime
    , teUpdated :: UTCTime
    , teDur :: NominalDiffTime
    , teUseStop :: Bool

    , teIsBillable :: Bool
    , teBillable :: Maybe Money
    , teCur :: Maybe Text

    , teTags :: [Text]
    } deriving (Eq, Show, Generic)

instance FromJSON TimeEntry where
    parseJSON = withObject "TimeEntry" $ \o -> TimeEntry
        <$> o .: "id"
        <*> o .: "uid"
        <*> o .: "user"
        <*> o .: "tid"
        <*> o .: "task"
        <*> o .: "pid"
        <*> o .: "project"
        <*> o .: "project_color"
        <*> o .: "project_hex_color"
        <*> o .: "client"
        <*> o .: "description"
        <*> o .: "start"
        <*> o .: "end"
        <*> o .: "updated"
        <*> ((/1000) <$> o .: "dur") -- Convert from milliseconds
        <*> o .: "use_stop"
        <*> o .: "is_billable"
        <*> o .: "billable"
        <*> o .: "cur"
        <*> o .: "tags"

instance ToJSON TimeEntry where
    toJSON TimeEntry{..} = object
        [ "id" .= teId
        , "uid" .= teUid
        , "user" .= teUser
        , "tid" .= teTid
        , "task" .= teTask
        , "pid" .= tePid
        , "project" .= teProject
        , "project_color" .= teProjectColor
        , "project_hex_color" .= teProjectHexColor
        , "client" .= teClient
        , "description" .= teDescription
        , "start" .= teStart
        , "end" .= teEnd
        , "updated" .= teUpdated
        , "dur" .= (round $ teDur * 1000 :: Integer) -- Convert to milliseconds
        , "use_stop" .= teUseStop
        , "is_billable" .= teIsBillable
        , "billable" .= teBillable
        , "cur" .= teCur
        , "tags" .= teTags
        ]

data TogglDetails = TogglDetails
    { tdData :: [TimeEntry]
    , tdTotalCount :: Integer
    , tdTotalGrand :: Integer
    , tdTotalBillable :: Maybe Money
    , tdPerPage :: Integer
    } deriving (Eq, Show, Generic)

instance FromJSON TogglDetails where
    parseJSON = withObject "TimeEntry" $ \o -> TogglDetails
        <$> o .: "data"
        <*> o .: "total_count"
        <*> o .: "total_grand"
        <*> o .: "total_billable"
        <*> o .: "per_page"

instance ToJSON TogglDetails where
    toJSON TogglDetails{..} = object
        [ "data" .= tdData
        , "total_count" .= tdTotalCount
        , "total_grand" .= tdTotalGrand
        , "total_billable" .= tdTotalBillable
        , "per_page" .= tdPerPage
        ]
