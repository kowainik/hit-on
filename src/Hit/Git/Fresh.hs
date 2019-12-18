-- | Everything related to the `hit fresh` command

module Hit.Git.Fresh
    ( runFresh
    ) where

import Shellmet()

import Hit.Git.Common (nameOrMaster)

-- | @hit fresh@ command.
runFresh :: Maybe Text -> IO ()
runFresh (nameOrMaster -> branch) = do
    "git" ["fetch", "origin", branch]
    "git" ["rebase", "origin/" <> branch]
