-- | Everything related to the `hit push` command

module Hit.Git.Push
    ( runPush
    ) where

import System.Process (callCommand)
import qualified Data.Text as T

import Hit.Core (PushBool (..))
import Hit.Git.Common (getCurrentBranch)

-- | @hit push@ command.
runPush :: PushBool -> IO ()
runPush isForce = getCurrentBranch >>= \branch ->
    callCommand $ "git push --set-upstream origin " ++ 
        (T.unpack branch) ++ 
            (if isForce == Force then " --force" else "")
