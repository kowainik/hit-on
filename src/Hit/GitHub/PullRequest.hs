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
    ( PrTitle (..)
    , queryPullRequests
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
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

queryPullRequests
    :: GH.GitHubToken
    -> Owner
    -> Repo
    -> Text
    -> IO (Either GH.GitHubError [PrTitle])
queryPullRequests token owner repo branch =
    GH.unNest @'[ "repository", "pullRequests", "nodes" ] $
    GH.queryGitHub
        token
        (GH.repositoryToAst $ pullRequestsQuery owner repo branch)

newtype PrTitle = PrTitle
    { unPriTitle :: Text
    } deriving stock (Show)
      deriving newtype (Eq)

instance FromJSON PrTitle
  where
    parseJSON = withObject "PrTitle" $ \o ->
        PrTitle <$> (o .: "title")
