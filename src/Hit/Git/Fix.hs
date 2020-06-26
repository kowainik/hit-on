{- |
Module                  : Hit.Git.Fix
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit fix@ command runner and helpers.
-}

module Hit.Git.Fix
    ( runFix
    ) where

import Shellmet ()

import Hit.Core (ForceFlag (..))
import Hit.Git.Push (runPush)


-- | @hit fix@ command
runFix :: Maybe Text -> ForceFlag -> IO ()
runFix msg forceFlag = do
    "git" ["add", "."]
    "git" ["commit", "-m", message]
    runPush forceFlag
  where
    message :: Text
    message = fromMaybe "Fix" msg
