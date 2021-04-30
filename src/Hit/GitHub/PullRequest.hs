{-# LANGUAGE DataKinds #-}

{- |
Module                  : Hit.GitHub.PullRequest
Copyright               : (c) 2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

PullRequest-related queries and data types.
-}

module Hit.GitHub.PullRequest
    ( queryPullRequests
    ) where

import Prolens (set)

import Hit.Core (Owner (..), Repo (..))

import qualified GitHub as GH


pullRequestsQuery :: Owner -> Repo -> Text -> GH.Repository
pullRequestsQuery (Owner owner) (Repo repo) branch = GH.repository
    ( GH.defRepositoryArgs
    & set GH.ownerL owner
    & set GH.nameL  repo
    )
    $ one
    $ GH.pullRequests
        ( GH.defPullRequestsArgs
        & set GH.lastL 1
        & set GH.statesL (one GH.open)
        & set GH.headRefNameL (Just branch)
        )
        (one $ GH.nodes $ one GH.title)

queryPullRequests :: GH.GitHubToken -> Owner -> Repo -> Text -> IO [Text]
queryPullRequests token owner repo branch =
    GH.unNested @'[ "repository", "pullRequests", "nodes", "title" ] <$>
    GH.queryGitHub
        token
        (GH.repositoryToAst $ pullRequestsQuery owner repo branch)
