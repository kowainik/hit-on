{- |
Module                  : Hit.GitHub.Issue
Copyright               : (c) 2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Issue-related queries and data types.
-}

module Hit.GitHub.Issue
    ( Issue (..)
    , queryIssue

    , ShortIssue (..)
    , queryIssueList
    , issueToShort

    , IssueTitle (..)
    , queryIssueTitle

    , IssueNumber (..)
    , mutationCreateNewIssue
    ) where

import Data.Aeson (Array, FromJSON (..), withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Prolens (set)

import Hit.Core (IssueOptions (..), Milestone (..), Owner (..), Repo (..))
import Hit.Git.Common (getUsername)
import Hit.GitHub.Repository (RepositoryField (..), RepositoryNode (..))
import Hit.GitHub.Milestone (MilestoneNumber)

import qualified Hit.Formatting as Fmt

import qualified Data.Text as Text
import qualified GitHub as GH

----------------------------------------------------------------------------
-- Big issue type
----------------------------------------------------------------------------

{- | Issue with all information about it.
-}
data Issue = Issue
    { issueTitle       :: Text
    , issueAuthorLogin :: Text
    , issueBody        :: Text
    , issueNumber      :: Int
    , issueUrl         :: Text
    , issueState       :: GH.IssueState
    , issueLabels      :: [Text]
    , issueAssignees   :: [Text]
    }

instance FromJSON Issue
  where
    parseJSON = withObject "Issue" $ \o -> do
        issueTitle       <- o .: "title"
        author           <- o .: "author"
        issueAuthorLogin <- author .: "login"
        issueBody        <- o .: "body"
        issueNumber      <- o .: "number"
        issueUrl         <- o .: "url"
        issueState       <- o .: "state"

        labels           <- o .: "labels"
        labelNodes       <- labels .: "nodes"
        issueLabels      <- parseLabels labelNodes

        assignees        <- o .: "assignees"
        assigneesNodes   <- assignees .: "nodes"
        issueAssignees   <- parseAssignees assigneesNodes

        pure Issue{..}
      where
        parseLabels :: Array -> Parser [Text]
        parseLabels = mapM (withObject "Label" $ \o -> o .: "name") . toList

{- | Query for the specific issue with full details.
-}
issueQuery :: Owner -> Repo -> Int -> GH.Repository
issueQuery (Owner owner) (Repo repo) issueNumber = GH.repository
    ( GH.defRepositoryArgs
    & set GH.ownerL owner
    & set GH.nameL  repo
    )
    $ one
    $ GH.issue
        ( GH.defIssueArgs
        & set GH.numberL issueNumber
        & set GH.statesL (one GH.open)
        & set GH.orderL
            ( Just $ GH.defOrder
            & set GH.fieldL GH.CreatedAt
            & set GH.directionL GH.Asc
            )
        )
        ( one
        $ GH.nodes
        $  GH.title
        :| [ GH.author $ one GH.login
           , GH.IssueBody
           , GH.IssueNumber
           , GH.IssueUrl
           , GH.IssueState
           , GH.IssueLabels
             $ GH.Labels
             ( GH.defLabelsArgs
             & set GH.lastL 5
             )
             (GH.nodes $ GH.one GH.LabelName)
           , GH.IssueAssignees
             $ GH.Assignees
             ( GH.defAssigneesArgs
             & set GH.lastL 5
             )
             (GH.nodes $ one GH.UserLogin)
           ]
        )

{- | Queries a single issue by number.
-}
queryIssue :: GH.GitHubToken -> Owner -> Repo -> Int -> IO Issue
queryIssue token owner repo issueNumber =
    unRepositoryField <$>
    GH.queryGitHub
        @(RepositoryField "issue" Issue)
        token
        (GH.repositoryToAst $ issueQuery owner repo issueNumber)

----------------------------------------------------------------------------
-- Small issue type
----------------------------------------------------------------------------

{- | GitHub issue with only small amount of information about it.
-}
data ShortIssue = ShortIssue
    { shortIssueNumber      :: Int
    , shortIssueTitle       :: Text
    , shortIssueAuthorLogin :: Text
    , shortIssueAssignees   :: [Text]
    , shortIssueMilestoneNumber :: Maybe MilestoneNumber
    }

instance FromJSON ShortIssue
  where
    parseJSON = withObject "Issue" $ \o -> do
        shortIssueTitle       <- o .: "title"
        author                <- o .: "author"
        shortIssueAuthorLogin <- author .: "login"
        shortIssueNumber      <- o .: "number"

        assignees           <- o .: "assignees"
        assigneesNodes      <- assignees .: "nodes"
        shortIssueAssignees <- parseAssignees assigneesNodes

        milestone           <- o .: "milestone"
        shortIssueMilestoneNumber <- milestone .:? "number"

        pure ShortIssue{..}

issueToShort :: Issue -> ShortIssue
issueToShort Issue{..} = ShortIssue
    { shortIssueNumber      = issueNumber
    , shortIssueTitle       = issueTitle
    , shortIssueAuthorLogin = issueAuthorLogin
    , shortIssueAssignees   = issueAssignees
    }

issueListQuery :: Owner -> Repo -> GH.Repository
issueListQuery (Owner owner) (Repo repo) = GH.repository
    ( GH.defRepositoryArgs
    & set GH.ownerL owner
    & set GH.nameL  repo
    )
    $ one
    $ GH.issues
        ( GH.defIssuesArgs
        & set GH.lastL 100
        & set GH.statesL (one GH.open)
        & set GH.orderL
            ( Just $ GH.defOrder
            & set GH.fieldL GH.CreatedAt
            & set GH.directionL GH.Asc
            )
        )
        ( one
        $ GH.nodes
        $  GH.title
        :| [ GH.author $ one GH.login
           , GH.IssueNumber
           , GH.IssueAssignees
             $ GH.Assignees
             ( GH.defAssigneesArgs
             & set GH.lastL 5
             )
             (GH.nodes $ one GH.UserLogin)
           , GH.IssueMilestone $ one GH.MilestoneNumber
           ]
        )

{- | Queries the latest 100 issues of the repository.
-}
queryIssueList :: GH.GitHubToken -> Owner -> Repo -> IO [ShortIssue]
queryIssueList token owner repo =
    unRepositoryNodes <$>
    GH.queryGitHub
        @(RepositoryNodes "issues" ShortIssue)
        token
        (GH.repositoryToAst $ issueListQuery owner repo)

----------------------------------------------------------------------------
-- Get only issue title
----------------------------------------------------------------------------

newtype IssueTitle = IssueTitle
    { unIssueTitle :: Text
    } deriving newtype (FromJSON)

issueTitleQuery :: Owner -> Repo -> Int -> GH.Repository
issueTitleQuery (Owner owner) (Repo repo) issueNumber = GH.repository
    ( GH.defRepositoryArgs
    & set GH.ownerL owner
    & set GH.nameL  repo
    )
    $ one
    $ GH.issue
        ( GH.defIssueArgs
        & set GH.numberL issueNumber
        )
        (one GH.title)

{- | Queries 'IssueTitle' by number.
-}
queryIssueTitle :: GH.GitHubToken -> Owner -> Repo -> Int -> IO IssueTitle
queryIssueTitle token owner repo issueNumber =
    GH.unNest @'[ "repository", "issue", "title" ] <$>
    GH.queryGitHub
        token
        (GH.repositoryToAst $ issueTitleQuery owner repo issueNumber)

----------------------------------------------------------------------------
-- Create new issue
----------------------------------------------------------------------------

{- | Data type to parse only issue number.
-}
newtype IssueNumber = IssueNumber
    { unIssueNumber :: Int
    }

instance FromJSON IssueNumber
  where
    parseJSON = withObject "IssueNumber" $ \o ->
        IssueNumber <$> (o .: "number")

{- | Query to create issue and return its number.
-}
createIssueMutation
    :: GH.RepositoryId
    -> Text  -- ^ Issue title
    -> Maybe GH.MilestoneId
    -> GH.CreateIssue
createIssueMutation repoId issueTitle milestoneId = GH.CreateIssue
    ( GH.defCreateIssueInput
    & set GH.repositoryIdL repoId
    & set GH.titleL issueTitle
    & setMilestone
    )
    [ GH.IssueNumber
    ]
  where
    setMilestone :: GH.CreateIssueInput fields -> GH.CreateIssueInput fields
    setMilestone = case milestoneId of
        Nothing  -> id
        Just mId -> set GH.milestoneIdL mId

mutationCreateNewIssue
    :: GH.GitHubToken
    -> Owner
    -> Repo
    -> Text
    -> Maybe Milestone
    -> IO IssueNumber
mutationCreateNewIssue token owner repo issueTitle _mMilestone = do
    repositoryId <- GH.queryRepositoryId token owner repo
    unRepositoryField <$>
    GH.mutationGitHub
        token
        (GH.createIssueToAst $ createIssueMutation repositoryId issueTitle Nothing)

----------------------------------------------------------------------------
-- Internals
----------------------------------------------------------------------------

parseAssignees :: Array -> Parser [Text]
parseAssignees = mapM (withObject "Assignee" $ \o -> o .: "login") . toList
