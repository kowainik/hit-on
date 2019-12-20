-- | Everything related to the `hit log` command

module Hit.Git.Log
    ( runLog
    ) where

import Shellmet()


-- | @hit log@ command.
runLog :: Maybe Text -> IO ()
runLog (fromMaybe "HEAD" -> commit)
    = "git" ["log", "--oneline", "--decorate", commit]
