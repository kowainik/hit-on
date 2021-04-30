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
queryMyId :: GH.GitHubToken -> IO GH.UserId
queryMyId token =
    fmap (GH.unNested @'[ "viewer" ])
    $ GH.queryGitHub token
    $ GH.viewerToAst
    $ GH.Viewer
    $ one GH.UserId

assignUserToIssue :: GH.GitHubToken -> GH.UserId -> GH.IssueId -> IO IssueNumber
assignUserToIssue token userId issueId =
    fmap (GH.unNested @'[ "number" ])
    $ GH.mutationGitHub token
    $ GH.addAssigneesToAssignableToAst
    $ GH.AddAssigneesToAssignable issueId [userId]
