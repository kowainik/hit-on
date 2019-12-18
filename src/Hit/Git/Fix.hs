-- | Everything related to the `hit fix` command

module Hit.Git.Fix
    ( runFix
    ) where

import Shellmet()

import Hit.Core (PushBool (..))
import Hit.Git.Push (runPush)

-- | @hit fix@ command
runFix :: Maybe Text -> PushBool -> IO ()
runFix msg pushBool = do
    "git" ["add", "."]
    "git" ["commit", "-m", message]
    runPush pushBool
  where
    message :: Text
    message = fromMaybe "Fix" msg
