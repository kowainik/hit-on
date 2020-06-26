{- |
Module                  : Hit.Git.Sync
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit sync@ command runner and helpers.
-}

module Hit.Git.Sync
    ( runSync
    ) where

import Shellmet ()

import Hit.Core (ForceFlag (..))
import Hit.Git.Common (getCurrentBranch)


-- | @hit sync@ command.
runSync :: ForceFlag -> IO ()
runSync forceFlag = getCurrentBranch >>= \branch -> case forceFlag of
    Simple -> "git" ["pull", "--rebase", "origin", branch]
    Force -> do
        "git" ["fetch", "origin", branch]
        "git" ["reset", "--hard", "origin/" <> branch]
