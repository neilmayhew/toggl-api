{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (encode)
import Options.Applicative

import qualified Data.ByteString.Lazy.Char8 as B

import Toggl.Fetch

main :: IO ()
main = do
  (params, period) <- execParser $
    info (helper <*> ((,) <$> parseFetchParams <*> parseFetchPeriod)) $
       header "FetchDetails - fetch Toggl details"
    <> progDesc "Use the Toggl API to download time entries for a given time period"
    <> fullDesc

  entries <- fetchEntries params period

  B.putStrLn $ encode entries
