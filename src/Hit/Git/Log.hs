-- | Everything related to the `hit log` command

module Hit.Git.Log
    ( runLog
    ) where

import qualified Data.Text as T
import System.Process (callCommand)

-- | @hit log@ command.
runLog :: Maybe Text -> IO ()
runLog (fromMaybe "HEAD" -> commit)
    = callCommand $ "git log --oneline --decorate " ++ (T.unpack commit)
