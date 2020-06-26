{- |
Module                  : Hit.Git.Fresh
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit fresh@ command runner and helpers.
-}

module Hit.Git.Fresh
    ( runFresh
    ) where

import Shellmet ()

import Hit.Git.Common (nameOrMaster)


-- | @hit fresh@ command.
runFresh :: Maybe Text -> IO ()
runFresh (nameOrMaster -> branch) = do
    "git" ["fetch", "origin", branch]
    "git" ["rebase", "origin/" <> branch]
