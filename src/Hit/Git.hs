{-# LANGUAGE ViewPatterns #-}

-- | Logic for CLI commands to make GitHub workflows easier.

module Hit.Git
       ( runHop
       , runFresh
       , runNew
       , runPush
       ) where

import Hit.ColorTerminal (errorMessage)
import Hit.Shell (($|))


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
            let branchName = login <> "/" <> show issueNum
            "git" ["checkout", "-b", branchName]

-- | @hit push@ command.
runPush :: IO ()
runPush = getCurrentBranch >>= \branch -> "git" ["push", "-u", "origin", branch]

nameOrMaster :: Maybe Text -> Text
nameOrMaster = fromMaybe "master"

-- | Get the name of the current branch.
getCurrentBranch :: IO Text
getCurrentBranch = "git" $| ["rev-parse", "--abbrev-ref", "HEAD"]
