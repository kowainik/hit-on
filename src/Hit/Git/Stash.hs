-- | Everything related to the `hit stash` command

module Hit.Git.Stash
    ( runStash
    ) where

import System.Process (callCommand)

-- | @hit stash@ command: save all local changes to stash.
runStash :: IO ()
runStash = do
    callCommand "git add ."
    callCommand "git stash"
