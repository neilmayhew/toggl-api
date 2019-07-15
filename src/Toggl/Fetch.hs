{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Toggl.Fetch where

import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Time
import Network.HTTP.Req hiding (header)
import Options.Applicative

import Toggl.Types

data FetchParams = FetchParams
  { optToken :: String
  , optWorkspace :: String
  , optClients :: [String]
  , optAgent :: Maybe String
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
  <*> ( optional $ argument auto
      $  metavar "SINCE"
      <> help "The starting date" )
  <*> ( optional $ argument auto
      $  metavar "UNTIL"
      <> help "The finishing date" )

fetchEntries :: FetchParams -> IO [TimeEntry]
fetchEntries FetchParams{..} = do

  aYearAgo <- addGregorianYearsRollOver (-1) . utctDay <$> getCurrentTime

  let
      token = pack optToken
      clientIds = case optClients of
        [] -> Nothing
        cs -> Just $ intercalate "," cs
      agent = optAgent <|> pure "fetchEntries"
      since = optSince <|> pure aYearAgo

      url = https "toggl.com" /: "reports" /: "api" /: "v2" /: "details"

      params =
        basicAuth token "api_token" <>
        "user_agent" =: agent <>
        "workspace_id" =: optWorkspace <>
        queryParam "client_ids" clientIds <>
        queryParam "since" since <>
        queryParam "until" optUntil

      fetch page = runReq def $
        responseBody <$> req GET url NoReqBody jsonResponse (params <> "page" =: page)

      loop p n = do
        details <- fetch p

        let p' = p + 1
            n' = n + tdPerPage details

        (tdData details :) <$>
          (if n' < tdTotalCount details
            then loop p' n'
            else pure [])

  concat <$> loop (1 :: Int) 0
