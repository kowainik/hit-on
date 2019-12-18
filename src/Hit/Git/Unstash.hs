-- | Everything related to the `hit unstash` command

module Hit.Git.Unstash
    ( runUnstash
    ) where

import System.Process (callCommand)

-- | @hit unstash@ command: pop all saved changes.
runUnstash :: IO ()
runUnstash = callCommand "git stash pop"
