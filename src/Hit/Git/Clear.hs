{- |
Module                  : Hit.Git.Clear
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit clear@ command runner and helpers.
-}

module Hit.Git.Clear
    ( runClear
    , clearWithPrompt
    ) where

import Colourista (infoMessage)
import Shellmet ()

import Hit.Core (ForceFlag (..))
import Hit.Prompt (Answer (..), prompt, yesOrNoText)


-- | Remove all local changes permanently.
runClear :: ForceFlag -> IO ()
runClear = clearWithPrompt clearChanges
    [ "This command permanently deletes all uncommited changes"
    , "Hint: if you want to save changes, use 'hit stash' command."
    , "Are you sure you want to delete changes? " <> yesOrNoText N
    ]
  where
    clearChanges :: IO ()
    clearChanges = do
        "git" ["add", "."]
        "git" ["reset", "--hard"]

-- | Clear guarded by 'ForceFlag'
clearWithPrompt
    :: IO ()  -- ^ Clearing action
    -> [Text]  -- ^ Long description
    -> ForceFlag
    -> IO ()
clearWithPrompt clear desc = \case
    Force  -> clear
    Simple -> do
        putText $ unlines desc
        prompt N >>= \case
            N -> infoMessage "Aborting clearing action"
            Y -> clear
