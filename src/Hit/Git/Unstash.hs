{- |
Module                  : Hit.Git.Unstash
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit unstash@ command runner and helpers.
-}

module Hit.Git.Unstash
    ( runUnstash
    ) where

import Shellmet ()


-- | @hit unstash@ command: pop all saved changes.
runUnstash :: IO ()
runUnstash = "git" ["stash", "pop"]
