-- | Everything related to the `hit unstash` command

module Hit.Git.Unstash
    ( runUnstash
    ) where

import Shellmet()

-- | @hit unstash@ command: pop all saved changes.
runUnstash :: IO ()
runUnstash = "git" ["stash", "pop"]
