-- | Everything related to the `hit fix` command

module Hit.Git.Fix
    ( runFix
    ) where

import qualified Data.Text as T
import System.Process (callCommand)

import Hit.Core (PushBool (..))
import Hit.Git.Push (runPush)

-- | @hit fix@ command
runFix :: Maybe Text -> PushBool -> IO ()
runFix msg pushBool = do
    callCommand "git add ."
    callCommand $ "git commit -m " <> T.unpack message
    runPush pushBool
  where
    message :: Text
    message = fromMaybe "Fix" msg
