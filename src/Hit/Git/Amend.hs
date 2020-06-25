-- | Everything related to the `hit amend` command

module Hit.Git.Amend
    ( runAmend
    ) where

import Shellmet ()

import Hit.Core (PushBool (..))
import Hit.Git.Push (runPush)


-- | @hit amend@ command.
runAmend :: Bool -> IO ()
runAmend localAmend = do
    "git" ["add", "."]
    "git" ["commit", "--amend", "--no-edit", "--date", "now"]
    unless localAmend $ runPush Force
