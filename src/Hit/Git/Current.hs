{- |
Module                  : Hit.Git.Current
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit current@ command runner and helpers.
-}

module Hit.Git.Current
    ( runCurrent
    ) where

import Colourista (formatWith, green)

import Hit.Git.Common (getCurrentBranch, issueFromBranch)
import Hit.Prompt (arrow)


{- | Part of the @hit current@ command. Prints the current branch and returns
the current issue number if possible.
-}
runCurrent :: IO (Maybe Int)
runCurrent = do
    branchName <- getCurrentBranch
    putTextLn $ arrow <> "Current branch: " <> formatWith [green] branchName
    pure $ issueFromBranch branchName
