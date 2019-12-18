-- | Everything related to the `hit commit` command

module Hit.Git.Commit
    ( runCommit
    ) where

import qualified Data.Text as T
import System.Process (callCommand)

import Hit.Core (CommitOptions (..), PushBool (..))
import Hit.Issue (getIssueTitle, mkIssueId)
import Hit.Git.Common (issueFromBranch, getCurrentBranch)
import Hit.ColorTerminal (errorMessage)
import Hit.Git.Push (runPush)

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
        callCommand "git add ."
        callCommand $ "git commit -m " ++ (T.unpack $ showMsg msg $ guard hasIssue *> issueNum)
        if (coPush || coIsForcePush == Force) then runPush coIsForcePush else pure ()

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
