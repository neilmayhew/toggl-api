{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Toggl.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Fixed (Centi)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)

type Money = Centi  -- Not enough precision for all currencies.
                    -- Some middle-eastern currencies use 3 decimal places, and
                    -- two use a single base-5 place. (Malagasy Ariary and Mauritanian Ouguiya)
                    -- Technically, one base-5 digit is log₁₀ 5 = 0.69897 base-10 digits.

-- Allow automatic Eq derivation for TimeEntry
instance Eq ZonedTime where
    (==) = (==) `on` zonedTimeToUTC

-- Documentation taken from https://github.com/toggl/toggl_api_docs/blob/master/reports/detailed.md

data TimeEntry = TimeEntry
    { teId :: Int               -- ^ id: time entry id

    , teUid :: Int              -- ^ uid: user id whose time entry it is
    , teUser :: Text            -- ^ user: full name of the user whose time entry it is

    , teTid :: Maybe Int        -- ^ tid: task id
    , teTask :: Maybe Text      -- ^ task: task name for which the time entry was recorded

    , tePid :: Int              -- ^ pid: project id
    , teProject :: Text         -- ^ project: project name for which the time entry was recorded

    , teProjectColor :: Text    -- ^ project_color: not documented
    , teProjectHexColor :: Text -- ^ project_hex_color: not documented

    , teClient :: Text          -- ^ client: client name for which the time entry was recorded

    , teDescription :: Text     -- ^ description: time entry description

    , teStart :: ZonedTime      -- ^ start: start time of the time entry in ISO 8601 date and time format (YYYY-MM-DDTHH:MM:SS)
    , teEnd :: ZonedTime        -- ^ end: end time of the time entry in ISO 8601 date and time format (YYYY-MM-DDTHH:MM:SS)
    , teUpdated :: ZonedTime    -- ^ updated: last time the time entry was updated in ISO 8601 date and time format (YYYY-MM-DDTHH:MM:SS)
    , teDur :: NominalDiffTime  -- ^ dur: time entry duration in milliseconds
    , teUseStop :: Bool         -- ^ use_stop: if the stop time is saved on the time entry, depends on user's personal settings

    , teIsBillable :: Bool      -- ^ is_billable: boolean, if the time entry was billable or not
    , teBillable :: Maybe Money -- ^ billable: billed amount
    , teCur :: Maybe Text       -- ^ cur: billable amount currency

    , teTags :: [Text]          -- ^ tags: array of tag names, which assigned for the time entry
    } deriving (Eq, Show, Generic)

instance Ord TimeEntry where
    compare = comparing teUser
           <> comparing teStartLocal
           <> comparing teEndLocal
           <> comparing teId

teStartLocal :: TimeEntry -> LocalTime
teStartLocal = zonedTimeToLocalTime . teStart

teEndLocal :: TimeEntry -> LocalTime
teEndLocal = zonedTimeToLocalTime . teEnd

teStartDay :: TimeEntry -> Day
teStartDay = localDay . teStartLocal

teEndDay :: TimeEntry -> Day
teEndDay = localDay . teEndLocal

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
    , tdTotalGrand :: Maybe Integer
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
