{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Toggl.Fetch where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time
import Network.HTTP.Req hiding (header)
import Options.Applicative hiding (action)
import Prelude hiding (until)
import System.IO (stderr)
import Text.Printf (hPrintf)

import Toggl.Types

data FetchParams = FetchParams
  { optToken :: String
  , optWorkspace :: String
  , optClients :: [String]
  , optAgent :: Maybe String
  , optTrace :: Bool
  , optSince :: Maybe Day
  , optUntil :: Maybe Day
  } deriving (Show)

parseFetchParams :: Parser FetchParams
parseFetchParams = FetchParams
  <$> ( strOption
      $  long "token"
      <> short 't'
      <> metavar "TOKEN"
      <> help "The API authorization token" )
  <*> ( strOption
      $  long "workspace"
      <> short 'w'
      <> metavar "WORKSPACE"
      <> help "The workspace ID" )
  <*> ( many $ strOption
      $  long "client"
      <> short 'c'
      <> metavar "CLIENT"
      <> help "Restrict results to this client (may be specified multiple times)" )
  <*> ( optional $ strOption
      $  long "agent"
      <> short 'a'
      <> metavar "AGENT"
      <> help "The user agent name to use" )
  <*> ( switch
      $  long "trace"
      <> help "Trace network operations" )
  <*> ( optional $ argument auto
      $  metavar "SINCE"
      <> help "The starting date" )
  <*> ( optional $ argument auto
      $  metavar "UNTIL"
      <> help "The finishing date" )

-- TODO: The API is retricted to periods no longer than a year.
-- When the period is greater than a year, break into multiple fetches.

fetchEntries :: FetchParams -> IO [TimeEntry]
fetchEntries FetchParams{..} = do

  today <- utctDay <$> getCurrentTime

  let
      aYearAgo = addDays 1 . addGregorianYearsClip (-1) $ today

      token = pack optToken
      clientIds = case optClients of
        [] -> Nothing
        cs -> Just $ intercalate "," cs
      agent = fromMaybe "haskell-toggl" optAgent
      since = fromMaybe aYearAgo optSince
      until = fromMaybe today optUntil

      url = https "toggl.com" /: "reports" /: "api" /: "v2" /: "details"

      -- https://github.com/toggl/toggl_api_docs/blob/master/reports/detailed.md
      -- https://github.com/toggl/toggl_api_docs/blob/master/reports.md#request-parameters

      params = basicAuth token "api_token"
        <> "user_agent" =: agent
        <> "workspace_id" =: optWorkspace
        <> queryParam "client_ids" clientIds
        <> "since" =: since
        <> "until" =: until

      fetch page = runReq def $
        responseBody <$> req GET url NoReqBody jsonResponse (params <> "page" =: page)

      loop p n t = do
        when optTrace $
          hPrintf stderr "Fetching page %d (%d/%d entries) ... " p n t

        (details, duration) <- timed $ fetch p

        when optTrace $
          hPrintf stderr "%s\n" (show duration)

        let p' = p + 1
            n' = n + tdPerPage details
            t' = tdTotalCount details

        (tdData details :) <$>
          (if n' >= t'
            then pure []
            else do
              threadDelay . round $ (1 - duration) * 1e6 -- Comply with rate-limiting
              loop p' n' t')

  concat <$> loop (1 :: Int) 0 1

timed :: (IO a) -> IO (a, NominalDiffTime)
timed action = do
  before <- getCurrentTime
  result <- action
  after <- getCurrentTime
  pure (result, after `diffUTCTime` before)
