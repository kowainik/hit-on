{- |
Module                  : Hit.Git.Amend
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit amend@ command runner and helpers.
-}

module Hit.Git.Amend
    ( runAmend
    ) where

import Shellmet ()

import Hit.Core (ForceFlag (..))
import Hit.Git.Push (runPush)


-- | @hit amend@ command.
runAmend :: Bool -> IO ()
runAmend localAmend = do
    "git" ["add", "."]
    "git" ["commit", "--amend", "--no-edit", "--date", "now"]
    unless localAmend $ runPush Force
