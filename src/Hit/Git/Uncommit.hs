{- |
Module                  : Hit.Git.Uncommit
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit uncommit@ command runner and helpers.
-}

module Hit.Git.Uncommit
    ( runUncommit
    ) where

import Shellmet ()


-- | @hit uncommit@ command
runUncommit :: IO ()
runUncommit = "git" ["reset", "HEAD~1"]
