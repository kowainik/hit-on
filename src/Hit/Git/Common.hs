{- | Functions which can be used by serveral Hit Commands 
and are not specific to one.
-}

module Hit.Git.Common 
    ( nameOrMaster
    , getUsername
    , getCurrentBranch
    , issueFromBranch
    , withDeletedFiles
    , withUntrackedFiles
    ) where

import qualified Data.Text as T

import Control.Exception (bracket)
import Data.Char (isDigit)
import Shellmet (($|))

import Hit.ColorTerminal (errorMessage)

nameOrMaster :: Maybe Text -> Text
nameOrMaster = fromMaybe "master"

-- | Get current user name from the local global git config.
getUsername :: IO Text
getUsername = do
    login <- "git" $| ["config", "user.login"]
    if login == ""
        then errorMessage "user.login is not specified" >> exitFailure
        else pure login

-- | Get the name of the current branch.
getCurrentBranch :: IO Text
getCurrentBranch = "git" $| ["rev-parse", "--abbrev-ref", "HEAD"]

{- | Extracts issue number from the branch in form like:

@
kowainik/<n>-short-description
@
-}
issueFromBranch :: Text -> Maybe Int
issueFromBranch =
      readMaybe
    . toString
    . T.takeWhile isDigit
    . T.drop 1
    . T.dropWhile (/= '/')

{- | Perform the given action by first staging the given files and
later removing them again after the action
-}
withFiles :: IO [Text] -> IO a -> IO a
withFiles whichFiles action = bracket
    addFiles
    removeFiles
    (const action)
  where
    addFiles :: IO [Text]
    addFiles = do
        files <- whichFiles
        for_ files $ \file -> void $ "git" $| ["add", file]
        pure files

    -- Return files back to not spoil git state and have unexpected behavior
    removeFiles :: [Text] -> IO ()
    removeFiles = mapM_ $ \file -> void $ "git" $| ["reset", "--", file]

{- | Perform given action by adding all deleted files to index and returning
them back after action.
-}
withDeletedFiles :: IO a -> IO a
withDeletedFiles = withFiles deletedFiles
  where
    -- Find the deleted file to index so they will appear in diff
    deletedFiles :: IO [Text]
    deletedFiles = lines <$> "git" $| ["ls-files", "--deleted", "--exclude-standard"]

{- | Perform given action by adding all untracked files to index and returning
them back after action.
-}
withUntrackedFiles :: IO a -> IO a
withUntrackedFiles = withFiles untrackedFiles
  where
    -- Find the untracked file to index so they will appear in diff
    untrackedFiles :: IO [Text]
    untrackedFiles = lines <$> "git" $| ["ls-files", "--others", "--exclude-standard"]
