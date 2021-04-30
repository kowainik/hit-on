{- |
Module                  : Hit.Git.Commit
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit commit@ command runner and helpers.
-}

module Hit.Git.Commit
    ( runCommit

      -- * Commit message helpers
    , toCommitMessage
    ) where

import Colourista (errorMessage)
import Shellmet ()

import Hit.Core (CommitOptions (..), ForceFlag (..), IssueNumber (..))
import Hit.Formatting (stripRfc)
import Hit.Git.Common (getCurrentBranch, issueFromBranch)
import Hit.Git.Issue (fetchIssueTitle)
import Hit.Git.Push (runPush)

import qualified Data.Text as T


-- | @hit commit@ command.
runCommit :: CommitOptions -> IO ()
runCommit CommitOptions{..} = case coName of
    Just (T.strip -> msg)
        | msg == "" -> errorMessage "Commit message cannot be empty" >> exitFailure
        | otherwise -> getCurrentIssue >>= commitCmds msg
    {- if the commit name is not specified then check the branchName
    If this is issue-related branch, take the issue name as the commit name.
    Otherwise print errorMessage.
    -}
    Nothing -> do
        issueNum <- getCurrentIssue
        case issueNum of
            Nothing -> do
                errorMessage "Commit message cannot be empty: can not be taken from the context"
                exitFailure
            Just n -> do
                title <- fetchIssueTitle n
                commitCmds title issueNum
  where
    commitCmds :: Text -> Maybe IssueNumber -> IO ()
    commitCmds msg issueNum = do
        "git" ["add", "."]
        "git" ["commit", "-m", toCommitMessage hasIssue msg issueNum]
        when (coPush || coIsForcePush == Force) $ runPush coIsForcePush

    getCurrentIssue :: IO (Maybe IssueNumber)
    getCurrentIssue = issueFromBranch <$> getCurrentBranch

    hasIssue :: Bool
    hasIssue = not coNoIssueNumber

toCommitMessage :: Bool -> Text -> Maybe IssueNumber -> Text
toCommitMessage hasIssue (stripRfc -> msg) issueNum
    | not hasIssue = msg
    | otherwise = case issueNum of
        Nothing -> msg
        Just (IssueNumber n) ->
            let issue = "#" <> show n
            in "[" <> issue <> "] " <> msg <> "\n\nResolves " <> issue
