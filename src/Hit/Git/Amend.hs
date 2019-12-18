-- | Everything related to the `hit amend` command

module Hit.Git.Amend
    ( runAmend
    ) where

import System.Process (callCommand)

import Hit.Core (PushBool (..))
import Hit.Git.Push (runPush)

-- | @hit amend@ command.
runAmend :: Bool -> IO ()
runAmend localAmend = do
    callCommand "git add ."
    callCommand "git commit --amend --no-edit"
    unless localAmend $ runPush Force
