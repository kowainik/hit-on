-- | Everything related to the `hit resolve` command

module Hit.Git.Resolve
    ( runResolve
    ) where

import Shellmet()

import Hit.Git.Hop (runHop)
import Hit.Git.Common (nameOrMaster, getCurrentBranch)


-- | @hit resolve@ command.
runResolve :: Maybe Text -> IO ()
runResolve (nameOrMaster -> master)= do
    curBranch <- getCurrentBranch
    runHop $ Just master
    when (curBranch /= master) $ "git" ["branch", "-D", curBranch]
