{- |
Module                  : Hit.Git.Resolve
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit resolve@ command runner and helpers.
-}

module Hit.Git.Resolve
    ( runResolve
    ) where

import Shellmet ()

import Hit.Git.Common (getCurrentBranch, nameOrMaster)
import Hit.Git.Hop (runHop)


-- | @hit resolve@ command.
runResolve :: Maybe Text -> IO ()
runResolve (nameOrMaster -> master)= do
    curBranch <- getCurrentBranch
    runHop $ Just master
    when (curBranch /= master) $ "git" ["branch", "-D", curBranch]
