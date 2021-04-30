{-# LANGUAGE DataKinds #-}

{- |
Module                  : Hit.GitHub.Issue
Copyright               : (c) 2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

User-related queries and data types.
-}

module Hit.GitHub.User
    ( queryMyId
    , assignUserToIssue
    ) where

import Hit.Core (IssueNumber)

import qualified GitHub as GH


-- TODO: move to @github-graphql@
queryMyId :: GH.GitHubToken -> IO (Either GH.GitHubError GH.UserId)
queryMyId token =
    GH.unNest @'[ "viewer" ]
    $ GH.queryGitHub token
    $ GH.viewerToAst
    $ GH.Viewer
    $ one GH.UserId

assignUserToIssue
    :: GH.GitHubToken
    -> GH.UserId
    -> GH.IssueId
    -> IO (Either GH.GitHubError IssueNumber)
assignUserToIssue token userId issueId =
    GH.unNest @'[ "number" ]
    $ GH.mutationGitHub token
    $ GH.addAssigneesToAssignableToAst
    $ GH.AddAssigneesToAssignable issueId [userId]
