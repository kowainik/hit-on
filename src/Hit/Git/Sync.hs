-- | Everything related to the `hit sync` command

module Hit.Git.Sync
    ( runSync
    ) where

import qualified Data.Text as T
import System.Process (callCommand)

import Hit.Git.Common (getCurrentBranch)

-- | @hit sync@ command.
runSync :: IO ()
runSync = getCurrentBranch >>= \branch ->
    callCommand $ "git pull --rebase origin " ++  (T.unpack branch)
