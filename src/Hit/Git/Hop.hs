-- | Everything related to the `hit hop` command

module Hit.Git.Hop 
    ( runHop
    ) where

import Shellmet()

import Hit.Git.Common (nameOrMaster)

-- | @hit hop@ command.
runHop :: Maybe Text -> IO ()
runHop (nameOrMaster -> branch) = do
    "git" ["checkout",  branch]
    "git" ["pull", "--rebase", "--prune"]
