{- |
Module                  : Hit.Error
Copyright               : (c) 2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Custom errors of the @hit@ tool.
-}

module Hit.Error
    ( HitError (..)
    , renderHitError
    ) where

data HitError
    = NoGitHubTokenEnv
    | InvalidOwnerRepo

renderHitError :: HitError -> Text
renderHitError = \case
    NoGitHubTokenEnv ->
        "The environment variable GITHUB_TOKEN is not set"
    InvalidOwnerRepo ->
        "Cannot not parse the 'owner' and 'repo' names from the owner/repo format"
