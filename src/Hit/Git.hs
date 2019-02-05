-- | Logic for CLI commands to make GitHub workflows easier.

module Hit.Git
       ( runHop
       ) where

import Hit.Shell ()


-- | @hit hop@ command
runHop :: Maybe Text -> IO ()
runHop branchName = do
    let branch = fromMaybe "master" branchName
    "git" ["checkout",  branch]
    "git" ["pull", "--rebase", "--prune"]
