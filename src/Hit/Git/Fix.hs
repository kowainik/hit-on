-- | Everything related to the `hit fix` command

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
