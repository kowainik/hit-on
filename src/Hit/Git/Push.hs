-- | Everything related to the `hit push` command

module Hit.Git.Push
    ( runPush
    ) where

import Shellmet()

import Hit.Core (PushBool (..))
import Hit.Git.Common (getCurrentBranch)

-- | @hit push@ command.
runPush :: PushBool -> IO ()
runPush isForce = getCurrentBranch >>= \branch ->
    "git" $ ["push", "--set-upstream", "origin", branch]
         ++ ["--force" | isForce == Force]
