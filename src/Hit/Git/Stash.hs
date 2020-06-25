{- |
Module                  : Hit.Git.Stash
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit stash@ command runner and helpers.
-}

module Hit.Git.Stash
    ( runStash
    ) where

import Shellmet ()


-- | @hit stash@ command: save all local changes to stash.
runStash :: IO ()
runStash = "git" ["stash", "--include-untracked"]
