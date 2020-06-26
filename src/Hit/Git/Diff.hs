{- |
Module                  : Hit.Git.Diff
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit diff@ command runner and helpers.
-}

module Hit.Git.Diff
    ( runDiff
    ) where

import Shellmet ()
import System.Directory (findExecutable)
import System.Process (callCommand)

import Hit.Git.Common (withUntrackedFiles)


{- | Show diff from the given commit. If commit is not specified, uses HEAD.
This commands checks whether @diff-hightligh@ is on path and if not, just calls
@git diff@.
-}
runDiff :: Maybe Text -> IO ()
runDiff (fromMaybe "HEAD" -> commit) = withUntrackedFiles $
    findExecutable "diff-highlight" >>= \case
        Nothing -> "git" ["diff", commit]
        Just _  -> callCommand $ toString $
            "git diff " <> commit <> " --color=always | diff-highlight | less -rFX"
