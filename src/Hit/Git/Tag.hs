{- |
Module                  : Hit.Git.Tag
Copyright               : (c) 2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit tag@ command runner and helpers.
-}

module Hit.Git.Tag
    ( runTag
    ) where

import Shellmet ()

import Hit.Core (TagAction (..), TagOptions (..))


-- | @hit tag@ command.
runTag :: TagOptions -> IO ()
runTag TagOptions{..} = case toAction of
    CreateTag -> do
        "git" ["tag", "-a", toName, "-m 'Tag for " <> toName <> " release'"]
        "git" ["push", "origin", "--tags"]
    DeleteTag -> do
        "git" ["tag", "-d", toName]
        "git" ["push", "--delete", "origin", toName]
