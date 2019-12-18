-- | Everything related to the `hit uncommit` command

module Hit.Git.Uncommit 
    ( runUncommit 
    ) where

import System.Process (callCommand)

-- | @hit uncommit@ command
runUncommit :: IO ()
runUncommit = callCommand "git reset HEAD~1"
