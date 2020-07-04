{- |
Module                  : Hit.Git.Stash
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Family of @hit stash@ and @hit unstash@ commands runners and helpers.
-}

module Hit.Git.Stash
    ( runStash
    , runStashClear
    , runStashDiff
    , runStashList
    , runUnstash
    ) where

import Shellmet ()

import Hit.Core (ForceFlag (..))
import Hit.Git.Clear (clearWithPrompt)
import Hit.Prompt (Answer (..), yesOrNoText)


-- | @hit stash@ command: save all local changes to stash.
runStash :: Maybe Text -> IO ()
runStash mName = do
    let args = case mName of
            Nothing   -> ["--include-untracked"]
            Just name -> ["push", "--include-untracked", "-m", name]
    "git" $ "stash" : args

-- | @hit stash clear@ command: clear all stashes.
runStashClear :: ForceFlag -> IO ()
runStashClear = clearWithPrompt clearCmd
    [ "This command permanently deletes all stashed changes"
    , "Are you sure you want to delete changes? " <> yesOrNoText N
    ]
  where
    clearCmd :: IO ()
    clearCmd = "git" ["stash", "list"]

-- | @hit stash diff@ command: show diff with the given stash.
runStashDiff :: Maybe Int -> IO ()
runStashDiff (fromMaybe 0 -> num) =
    "git" ["stash", "show", "-p", "stash@{" <> show num <> "}"]

-- | @hit stash list@ command: list all stashes.
runStashList :: IO ()
runStashList = "git" ["stash", "list"]

-- | @hit unstash@ command: pop all saved changes.
runUnstash :: IO ()
runUnstash = "git" ["stash", "pop"]
