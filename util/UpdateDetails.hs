{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (filterM, when)
import Data.Aeson (encode)
import Data.Time
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit
import System.IO (hPutStr, stderr)

import qualified Data.ByteString.Lazy.Char8 as B

import Toggl.Fetch
import Toggl.Main
import Toggl.Types

main :: IO ()
main = do
  (fetchParams, UpdateParams{..}) <- execParser $
    info (helper <*> ((,)
        <$> parseFetchParams
        <*> parseUpdateParams)) $
       header "UpdateDetails - update Toggl details"
    <> progDesc "Use the Toggl API to download time entries and add them to a file"
    <> fullDesc

  (errs, existing) <- loadFiles =<< filterM doesFileExist [updateFilename]

  when (not $ null errs) $ do
    hPutStr stderr . unlines $
      "Error loading existing entries:" : errs
    exitFailure

  let cutoff = if null existing
          then updateEpoch
          else addDays (-updateWindow) . maximum $ map teStartDay existing

  today <- localDay . zonedTimeToLocalTime <$> getZonedTime

  updates <- fetchEntries fetchParams $ FetchPeriod (Just cutoff) (Just today)

  let old = filter ((< cutoff) . teStartDay) existing
      new = updates ++ old -- Toggl returns newest-first

  when (existing /= new) $
    B.writeFile updateFilename $ encode new

data UpdateParams = UpdateParams
  { updateFilename :: FilePath
  , updateEpoch :: Day
  , updateWindow :: Integer
  } deriving (Show)

parseUpdateParams :: Parser UpdateParams
parseUpdateParams = UpdateParams
  <$> ( strOption
      $  long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "The file that entries are stored in" )
  <*> ( option auto
      $  long "epoch"
      <> short 'e'
      <> metavar "DATE"
      <> help "Fetch entries from DATE onwards if there are no existing entries" )
  <*> ( option auto
      $  long "update-window"
      <> short 'u'
      <> metavar "DAYS"
      <> value 6
      <> help "Re-fetch all entries from DAYS ago onwards [6]" )
