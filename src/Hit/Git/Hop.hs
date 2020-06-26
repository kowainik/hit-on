{- |
Module                  : Hit.Git.Hop
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit hop@ command runner and helpers.
-}

module Hit.Git.Hop
    ( runHop
    ) where

import Shellmet ()

import Hit.Git.Common (nameOrMaster)


-- | @hit hop@ command.
runHop :: Maybe Text -> IO ()
runHop (nameOrMaster -> branch) = do
    "git" ["checkout",  branch]
    "git" ["pull", "--rebase", "--prune"]
