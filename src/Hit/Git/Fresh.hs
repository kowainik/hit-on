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

import Colourista (errorMessage, infoMessage, warningMessage)
import Shellmet (($?))
import System.IO (hFlush)

import Hit.Git.Common (branchOrMain)
import Hit.Prompt (Answer (..), arrow, prompt, yesOrNoText)


-- | @hit fresh@ command.
runFresh :: Maybe Text -> IO ()
runFresh mBranch = do
    branch <- branchOrMain mBranch
    "git" ["fetch", "origin", branch]
    isRebase <- (True <$ "git" ["rebase", "origin/" <> branch]) $? pure False
    unless isRebase $ do
        errorMessage " 'git rebase' failed."
        putTextLn $ "  Do you want to abort the command? " <> yesOrNoText Y
        putText arrow >> hFlush stdout
        prompt Y >>= \case
            Y -> do
                infoMessage "Aborting the command."
                "git" ["rebase", "--abort"] $? pass
            N -> warningMessage "Unsuccessful 'hit fresh' while rebasing."
