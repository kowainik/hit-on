-- | Everything related to the `hit fresh` command

module Hit.Git.Fresh
    ( runFresh
    ) where

import qualified Data.Text as T
import System.Process (callCommand)

import Hit.Git.Common (nameOrMaster)

-- | @hit fresh@ command.
runFresh :: Maybe Text -> IO ()
runFresh (nameOrMaster -> branch) = do
    callCommand $ "git fetch origin " ++ (T.unpack branch)
    callCommand $ "git rebase origin/" ++ (T.unpack branch)
