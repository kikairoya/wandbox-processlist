{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Prelude
import qualified Data.Text as T
import Data.Maybe
import qualified System.Process as Process
import qualified Text.Regex as Regex

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        processes <- liftIO $ enumProcesses
        setTitle "Wandbox Process List"
        $(widgetFile "homepage")

enumProcesses :: IO (Text,[[Text]])
enumProcesses = do
  summary <- T.pack . foldr1 ((++) . flip (++) "\n") . take 4 . tail . lines <$> Process.readProcess "top" ["-b", "-n1", "-u", "wandbox"] ""
  procs <- map T.words . catMaybes . map replaceCmd . lines <$> Process.readProcess "ps" ["-uwandbox", "-opid,thcount,start,cputime,pcpu,pmem,comm"] ""
  return (summary, procs)
  where
    replaceCmd line
      | isJust $ Regex.matchRegex (Regex.mkRegex " su *$") line = Nothing
      | isJust $ Regex.matchRegex (Regex.mkRegex " server.exe *$") line = Just $ T.replace "server.exe" "cattleshed" $ T.pack line
      | otherwise = Just $ T.pack line
