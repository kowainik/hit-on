-- | Everything related to the `hit stash` command

module Hit.Git.Stash
    ( runStash
    ) where

import Shellmet()


-- | @hit stash@ command: save all local changes to stash.
runStash :: IO ()
runStash = "git" ["stash", "--include-untracked"]
