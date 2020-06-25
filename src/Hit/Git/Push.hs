-- | Everything related to the `hit push` command

module Hit.Git.Push
    ( runPush
    ) where

import Shellmet ()

import Hit.Core (ForceFlag (..))
import Hit.Git.Common (getCurrentBranch)


-- | @hit push@ command.
runPush :: ForceFlag -> IO ()
runPush forceFlag = getCurrentBranch >>= \branch ->
    "git" $ ["push", "--set-upstream", "origin", branch]
         ++ ["--force" | forceFlag == Force]
