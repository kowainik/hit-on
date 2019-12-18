-- | Everything related to the `hit resolve` command

module Hit.Git.Resolve
    ( runResolve
    ) where

import qualified Data.Text as T
import System.Process (callCommand)

import Hit.Git.Hop (runHop)
import Hit.Git.Common (nameOrMaster, getCurrentBranch)

-- | @hit resolve@ command.
runResolve :: Maybe Text -> IO ()
runResolve (nameOrMaster -> master)= do
    curBranch <- getCurrentBranch
    runHop $ Just master
    when (curBranch /= master) $ 
        callCommand $ "git branch -D " ++ (T.unpack curBranch)
