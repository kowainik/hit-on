-- | Everything related to the `hit uncommit` command

module Hit.Git.Uncommit
    ( runUncommit
    ) where

import Shellmet()

-- | @hit uncommit@ command
runUncommit :: IO ()
runUncommit = "git" ["reset", "HEAD~1"]
