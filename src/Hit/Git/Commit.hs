-- | Everything related to the `hit commit` command

module Hit.Git.Commit
    ( runCommit
    ) where

import Shellmet ()

import Colourista (errorMessage)

import Hit.Core (CommitOptions (..), PushBool (..))
import Hit.Git.Common (getCurrentBranch, issueFromBranch)
import Hit.Git.Push (runPush)
import Hit.Issue (getIssueTitle, mkIssueId)

import qualified Data.Text as T


-- | @hit commit@ command.
runCommit :: CommitOptions -> IO ()
runCommit CommitOptions{..} = case coName of
    Just (T.strip -> msg)
        | msg == "" -> errorMessage "Commit message cannot be empty"
        | otherwise -> getCurrentIssue >>= commitCmds msg
    {- if the commit name is not specified then check the branchName
    If this is issue-related branch, take the issue name as the commit name.
    Otherwise print errorMessage.
    -}
    Nothing -> do
        issueNum <- getCurrentIssue
        case issueNum of
            Nothing -> errorMessage "Commit message cannot be empty: can not be taken from the context"
            Just n -> do
                title <- getIssueTitle (mkIssueId n)
                commitCmds title issueNum
  where
    commitCmds :: Text -> Maybe Int -> IO ()
    commitCmds msg issueNum = do
        "git" ["add", "."]
        "git" ["commit", "-m", showMsg msg $ guard hasIssue *> issueNum]
        when (coPush || coIsForcePush == Force) $ runPush coIsForcePush

    getCurrentIssue :: IO (Maybe Int)
    getCurrentIssue = issueFromBranch <$> getCurrentBranch

    showMsg :: Text -> Maybe Int -> Text
    showMsg msg = \case
       Nothing -> msg
       Just n  ->
           let issue = "#" <> show n
           in "[" <> issue <> "] " <> msg <> "\n\nResolves " <> issue

    hasIssue :: Bool
    hasIssue = not coNoIssueNumber
