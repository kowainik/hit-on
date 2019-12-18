-- | Everything related to the `hit clear` command

module Hit.Git.Clear
    ( runClear
    ) where

import Shellmet()

import Hit.ColorTerminal (Answer (..), infoMessage, prompt, yesOrNoText)
import Hit.Core (PushBool (..))

-- | Remove all local changes permanently.
runClear :: PushBool -> IO ()
runClear = \case
    Force  -> clearChanges
    Simple -> do
        putText $ unlines
            [ "This command permanently deletes all uncommited changes"
            , "Hint: if you want to save changes, use 'hit stash' command."
            , "Are you sure you want to delete changes? " <> yesOrNoText N
            ]
        prompt N >>= \case
            N -> infoMessage "Aborting local clean up"
            Y -> clearChanges
  where
    clearChanges :: IO ()
    clearChanges = do
        "git" ["add", "."]
        "git" ["reset", "--hard"]
