-- | Everything related to the `hit sync` command

module Hit.Git.Sync
    ( runSync
    ) where

import Shellmet ()

import Hit.Core (ForceFlag (..))
import Hit.Git.Common (getCurrentBranch)


-- | @hit sync@ command.
runSync :: ForceFlag -> IO ()
runSync forceFlag = getCurrentBranch >>= \branch -> case forceFlag of
    Simple -> "git" ["pull", "--rebase", "origin", branch]
    Force -> do
        "git" ["fetch", "origin", branch]
        "git" ["reset", "--hard", "origin/" <> branch]
