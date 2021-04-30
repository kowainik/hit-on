{- |
Module                  : Hit.Core
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains core data types used in the package.
-}

module Hit.Core
    ( -- * Wrapper types
      Owner (..)
    , Repo (..)
    , IssueNumber (..)

    , ForceFlag (..)
    , CommitOptions (..)
      -- * @hit issue@
    , IssueOptions (..)
    , defaultIssueOptions
      -- * Milestones
    , MilestoneOption (..)
      -- * @hit new@
    , NewOptions (..)
    , newOptionsWithName

    , TagOptions (..)
    , TagAction (..)
    ) where

import Data.Aeson (FromJSON)


newtype Owner = Owner
    { unOwner :: Text
    } deriving stock (Show)
      deriving newtype (Eq)

newtype Repo = Repo
    { unRepo :: Text
    } deriving stock (Show)
      deriving newtype (Eq)

{- | Safe wrapper for issue number.
-}
newtype IssueNumber = IssueNumber
    { unIssueNumber :: Int
    } deriving stock (Show)
      deriving newtype (Eq, FromJSON)

{- | Data type to represent the type of @push@ or @sync@: force-push
(force-reset) or not.
-}
data ForceFlag
    = Simple
    | Force
    deriving stock (Show, Eq)

-- | Options of the @hit commit@ command.
data CommitOptions = CommitOptions
    { {- | Commit name. If not specified use the issue name.
        If issue number is not applicable do not perform any actions.
        -}
      coName          :: !(Maybe Text)
      -- | Do not use the issue num in the commit name.
    , coNoIssueNumber :: !Bool
      -- | Push immediately.
    , coPush          :: !Bool
      -- | Use Force push?
    , coIsForcePush   :: !ForceFlag
    }

-- | Options of the @hit issue@ command.
data IssueOptions = IssueOptions
    { ioIssueNumber :: !(Maybe IssueNumber)
    , ioMe          :: !Bool
    , ioMilestone   :: !(Maybe MilestoneOption)
    }

-- | Internal representation of the GutHub Milestone Number in CLI.
data MilestoneOption
    = CurrentMilestone
    | MilestoneNum !Int
    deriving stock (Show)

defaultIssueOptions :: IssueOptions
defaultIssueOptions = IssueOptions
    { ioIssueNumber = Nothing
    , ioMe = False
    , ioMilestone = Nothing
    }


data NewOptions = NewOptions
    { noCreateIssue   :: !Bool  -- ^ Should create issue as well?
    , noIssueOrBranch :: !(Maybe Text)  -- ^ Issue or branch name
    , noMe            :: !Bool  -- ^ Branch from __my__ issues?
    , noMilestone     :: !(Maybe MilestoneOption)  -- ^ When creating a new issue, add to any milestone?
    }

newOptionsWithName :: Text -> NewOptions
newOptionsWithName issueOrBranch = NewOptions
    { noCreateIssue = False
    , noMe = False
    , noIssueOrBranch = Just issueOrBranch
    , noMilestone = Nothing
    }


-- | @tag@ command arguments
data TagOptions = TagOptions
    { toName   :: !Text
    , toAction :: !TagAction
    } deriving stock (Show)

-- | Possible user Actions with tags.
data TagAction
    = CreateTag
    | DeleteTag
    deriving stock (Show)
