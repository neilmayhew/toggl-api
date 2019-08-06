module Toggl.Main where

import Control.Monad (unless)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Bitraversable (bitraverse)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Toggl.Types (TimeEntry)

handleFiles :: ([TimeEntry] -> IO a) -> [FilePath] -> IO ([String], a)
handleFiles action files =
    bitraverse pure (action . concat) . partitionEithers =<<
    traverse eitherDecodeFileStrict files

loadFiles :: [FilePath] -> IO ([String], [TimeEntry])
loadFiles = handleFiles pure

mainFilesWith :: ([TimeEntry] -> IO Bool) -> [FilePath] -> IO ()
mainFilesWith action files = do
    (errs, success) <- handleFiles action files
    traverse_ (hPutStrLn stderr) errs
    unless (null errs && success) exitFailure

mainWith :: ([TimeEntry] -> IO Bool) -> IO ()
mainWith action = mainFilesWith action =<< getArgs
