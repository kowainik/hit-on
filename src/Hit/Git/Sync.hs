-- | Everything related to the `hit sync` command

module Hit.Git.Sync
    ( runSync
    ) where

import Shellmet()

import Hit.Git.Common (getCurrentBranch)

-- | @hit sync@ command.
runSync :: IO ()
runSync = getCurrentBranch >>= \branch ->
    "git" ["pull", "--rebase", "origin", branch]
