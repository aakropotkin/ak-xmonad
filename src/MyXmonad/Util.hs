-- Helpers and some functions scraped from XMonad's internal Core

module MyXmonad.Util (
  lookupDirEnv, lookupDirsEnv
)where

import System.IO (FilePath)
import System.Environment (lookupEnv)
import System.Directory (doesDirectoryExist)
import Data.Text (pack, unpack, splitOn)
--import Data.Text (pack, unpack)
import Control.Monad (filterM)
import System.Process (readProcess)
--import Data.List.Split (splitOn)

-- Selects the first match
lookupDirEnv :: String -> IO (Maybe FilePath)
lookupDirEnv var = do
  fps <- lookupDirsEnv var
  case fps of
    []   -> return Nothing
    fp:_ -> return $ Just fp

lookupDirsEnv :: String -> IO [FilePath]
lookupDirsEnv var = do
  res <- lookupEnv var
  case res of
    Nothing    -> return []
    (Just fps) ->
      let
        sliced :: [String]
        sliced = strSplitOn ":" fps
      in
      filterM doesDirectoryExist sliced

-- Steal `Data.Text.(split, splitOn)`
-- strSplit :: (Char -> Bool) -> String -> [String]
-- strSplit p = map unpack . split p . pack

strSplitOn :: String -> String -> [String]
strSplitOn s = map unpack . splitOn (pack s) . pack

xrdbQuery :: String -> IO String
xrdbQuery q = do
  xrdb <- readProcess "xrdb" ["-query"] []
  let xrdblns = lines xrdb
  return xrdb
