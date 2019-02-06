{-# LANGUAGE ViewPatterns #-}

-- | Logic for CLI commands to make GitHub workflows easier.

module Hit.Git
       ( runHop
       , runFresh
       , runNew
       , runPush
       , runResolve
       , runCommit
       , runAmend
       , runSync
       , runCurrent
       ) where

import Data.Char (isAlphaNum, isDigit, isSpace)

import Hit.ColorTerminal (arrow, errorMessage, greenCode, resetCode)
import Hit.Issue (getIssueTitle, mkIssueId)
import Hit.Shell (($|))

import qualified Data.Text as T


-- | @hit hop@ command.
runHop :: Maybe Text -> IO ()
runHop (nameOrMaster -> branch) = do
    "git" ["checkout",  branch]
    "git" ["pull", "--rebase", "--prune"]

-- | @hit fresh@ command.
runFresh :: Maybe Text -> IO ()
runFresh (nameOrMaster -> branch) = do
    "git" ["fetch", "origin", branch]
    "git" ["rebase", "origin/" <> branch]

-- | @hit new@ command.
runNew :: Int -> IO ()
runNew issueNum = do
    login <- "git" $| ["config", "user.login"]
    if login == ""
        then errorMessage "user.login is not specified"
        else do
            let issueId = mkIssueId issueNum
            issueTitle <- getIssueTitle issueId
            let shortDesc = mkShortDesc issueTitle
            let branchName = login <> "/" <> show issueNum <> "-" <> shortDesc
            "git" ["checkout", "-b", branchName]
  where
    mkShortDesc :: Text -> Text
    mkShortDesc =
          T.intercalate "-"
        . take 5
        . words
        . T.filter (\c -> isAlphaNum c || isDigit c || isSpace c)

-- | @hit commit@ command.
runCommit :: Text -> Bool -> IO ()
runCommit (T.strip -> msg) (not -> hasIssue)
    | msg == "" = errorMessage "Commit message cannot be empty"
    | otherwise = do
        branch <- getCurrentBranch
        let issueNum = issueFromBranch branch
        "git" ["add", "."]
        "git" ["commit", "-m", showMsg $ guard hasIssue *> issueNum]
  where
    showMsg :: Maybe Int -> Text
    showMsg = \case
       Nothing -> msg
       Just n  ->
           let issue = "#" <> show n
           in "[" <> issue <> "] " <> msg <> "\n\nResolves " <> issue

-- | @hit amend@ command.
runAmend :: IO ()
runAmend = do
    "git" ["add", "."]
    "git" ["commit", "--amend", "--no-edit"]
    runPush True

-- | @hit push@ command.
runPush :: Bool -> IO ()
runPush isForce = getCurrentBranch >>= \branch ->
    "git" $ ["push", "--set-upstream", "origin", branch]
         ++ ["--force" | isForce]

-- | @hit sync@ command.
runSync :: IO ()
runSync = getCurrentBranch >>= \branch -> "git" ["pull", "--rebase", "origin", branch]

-- | @hit resolve@ command.
runResolve :: Maybe Text -> IO ()
runResolve (nameOrMaster -> master)= do
    curBranch <- getCurrentBranch
    runHop $ Just master
    when (curBranch /= master) $ "git" ["branch", "-D", curBranch]

{- | Part of the @hit current@ command. Prints the current branch and returns
the current issue number if possible.
-}
runCurrent :: IO (Maybe Int)
runCurrent = do
    branchName <- getCurrentBranch
    putTextLn $ arrow <> "Current branch: " <> greenCode <> branchName <> resetCode
    pure $ issueFromBranch branchName

----------------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------------

nameOrMaster :: Maybe Text -> Text
nameOrMaster = fromMaybe "master"

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
