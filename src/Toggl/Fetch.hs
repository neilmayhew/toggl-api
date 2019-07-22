{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Toggl.Fetch where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Bifunctor (first, second)
import Data.ByteString.Char8 (pack, unpack)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Time
import Network.HTTP.Req hiding (header)
import Options.Applicative hiding (action)
import Options.Applicative.Types (readerAsk)
import Prelude hiding (until)
import System.IO (stderr)
import Text.Printf (hPrintf)

import Toggl.Types

data FetchParams = FetchParams
  { optToken :: String
  , optWorkspace :: String
  , optQueries :: [(String, String)]
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
  <*> ( many $ option qp
      $  long "query"
      <> short 'q'
      <> metavar "PARAM=VALUE"
      <> help "Use this additional query parameter (may be specified multiple times)" )
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
  where
    qp = second (drop 1) . span (/= '=') <$> readerAsk

-- TODO: The API is retricted to periods no longer than a year.
-- When the period is greater than a year, break into multiple fetches.

fetchEntries :: FetchParams -> IO [TimeEntry]
fetchEntries FetchParams{..} = do

  today <- utctDay <$> getCurrentTime

  let
      aYearAgo = addDays 1 . addGregorianYearsClip (-1) $ today

      token = pack optToken
      queries = foldMap (uncurry (=:) . first fromString) optQueries
      agent = fromMaybe "haskell-toggl" optAgent
      since = fromMaybe aYearAgo optSince
      until = fromMaybe today optUntil

      url = https "toggl.com" /: "reports" /: "api" /: "v2" /: "details"

      -- https://github.com/toggl/toggl_api_docs/blob/master/reports/detailed.md
      -- https://github.com/toggl/toggl_api_docs/blob/master/reports.md#request-parameters

      params = basicAuth token "api_token"
        <> "user_agent" =: agent
        <> "workspace_id" =: optWorkspace
        <> queries
        <> "since" =: since
        <> "until" =: until

      fetch page = do
        resp <- runReq def $ req GET url NoReqBody jsonResponse (params <> "page" =: page)
        for_ (responseHeader resp "Warning") $ \msg ->
          hPrintf stderr "Warning: %s\n" $ unpack msg :: IO ()
        pure $ responseBody resp

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
