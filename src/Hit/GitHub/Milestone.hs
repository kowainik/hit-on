{- |
Module                  : Hit.GitHub.Milestone
Copyright               : (c) 2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Milestone-related queries and data types.
-}

module Hit.GitHub.Milestone
    ( MilestoneNumber
    , queryLatestMilestoneNumber

    , Milestone (..)
    , queryMilestoneList
    ) where

import Prolens (set)

import Hit.Core (Owner (..), Repo (..))
import Hit.GitHub.Repository (RepositoryNodes (..))

import qualified GitHub as GH

----------------------------------------------------------------------------
-- Milestone number
----------------------------------------------------------------------------

newtype MilestoneNumber = MilestoneNumber
    { unMilestoneNumber :: Int
    } deriving newtype (FromJSON)

newtype LatestMilestone = LatestMilestone
    { unLatestMilestone :: MilestoneNumber
    }

instance FromJSON LatestMilestone
  where
    parseJSON = withObject "LatestMilestone" $ \o ->
        LatestMilestone <$> (o .: "number")

latestMilestoneQuery :: Owner -> Repo -> GH.Repository
latestMilestoneQuery (Owner owner) (Repo repo) = GH.repository
    ( GH.defRepositoryArgs
    & set GH.ownerL owner
    & set GH.nameL  repo
    )
    $ one
    $ GH.milestones
        ( GH.defMilestonesArgs
        & set GH.lastL 1
        & set GH.orderL
            ( Just $ GH.defOrder
            & set GH.fieldL GH.MNumber
            & set GH.directionL GH.Desc
            )
        )
        (one $ GH.nodes $ one MilestoneNumber)

{- | Query the number of the latest milestone.
-}
queryLatestMilestoneNumber :: GH.GitHubToken -> Owner -> Repo -> IO (Maybe MilestoneNumber)
queryLatestMilestoneNumber token owner repo =
    RepositoryNodes milestones <- GH.queryGitHub
        @(RepositoryNodes "milestones" LatestMilestone)
        token
        (GH.repositoryToAst $ latestMilestoneQuery owner repo)

    pure $ case milestones of
        [] -> Nothing
        m:_ -> Just $ unLatestMilestone m

----------------------------------------------------------------------------
-- Full milestone
----------------------------------------------------------------------------

data Milestone = Milestone
    { milestoneId                 :: Text
    , milestoneNumber             :: MilestoneNumber
    , milestoneTitle              :: Text
    , milestoneProgressPercentage :: Double
    , milestoneTotalIssues        :: Int
    } deriving stock (Show, Eq)

instance FromJSON Milestone
  where
    parseJSON = withObject "Milestone" $ \o -> do
        milestoneId    <- o .: "id"
        milestoneNumber <- o .: "number"
        milestoneTitle  <- o .: "title"

        milestoneProgressPercentage <- o .: "progressPercentage"

        issues <- o .: "issues"
        milestoneTotalIssues <- issues .: "totalCount"

        pure Milestone{..}

milestonesQuery :: Owner -> Repo -> GH.Repository
milestonesQuery (Owner owner) (Repo repo) = GH.repository
    ( GH.defRepositoryArgs
    & set GH.ownerL owner
    & set GH.nameL  repo
    )
    $ one
    $ GH.milestones
        ( GH.defMilestonesArgs
        & set GH.lastL 100
        & set GH.orderL
            ( Just $ GH.defOrder
            & set GH.fieldL GH.CreatedAt
            & set GH.directionL GH.Desc
            )
        )
        ( one
        $ GH.nodes
        $    GH.MilestoneId
        :| [ GH.MilestoneNumber
           , GH.MilestoneProgressPercentage
           , GH.MilestoneTitle
           , GH.MilestoneIssues $ GH.Issues
               ( GH.defIssuesArgs
               & set GH.lastL 1000
               & set GH.statesL (universeNonEmpty @GH.IssueState)
               )
               (one GH.TotalCount)
           ]
        )

{- | Queries the latest 100 issues of the repository.
-}
queryMilestoneList :: GH.GitHubToken -> Owner -> Repo -> IO [Milestone]
queryMilestoneList token owner repo =
    unRepositoryNodes <$>
    GH.queryGitHub
        @(RepositoryNodes "milestones" Milestone)
        token
        (GH.repositoryToAst $ milestonesQuery owner repo)
