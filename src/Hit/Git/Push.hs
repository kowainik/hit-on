{- |
Module                  : Hit.Git.Push
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit push@ command runner and helpers.
-}

module Hit.Git.Push
    ( runPush
    ) where

import Shellmet ()

import Hit.Core (ForceFlag (..))
import Hit.Git.Common (getCurrentBranch)


-- | @hit push@ command.
runPush :: ForceFlag -> IO ()
runPush forceFlag = getCurrentBranch >>= \branch ->
    "git" $ ["push", "--set-upstream", "origin", branch]
         ++ ["--force" | forceFlag == Force]
