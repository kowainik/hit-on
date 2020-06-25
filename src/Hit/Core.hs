{- | This module contains core data types used in the package.
-}

module Hit.Core
       ( PushBool (..)
       , CommitOptions (..)
       , IssueOptions (..)
       , Milestone (..)
       , defaultIssueOptions
       ) where


-- | Data type to represent the type of @push@: force-push or not.
data PushBool
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
    , coIsForcePush   :: !PushBool
    }

-- | Options of the @hit issue@ command.
data IssueOptions = IssueOptions
    { ioIssueNumber :: !(Maybe Int)
    , ioMe          :: !Bool
    , ioMilestone   :: !(Maybe Milestone)
    }

-- | Internal representation of the GutHub Milestone in CLI.
data Milestone
    = CurrentMilestone
    | MilestoneId !Int
    deriving stock (Show)

defaultIssueOptions :: IssueOptions
defaultIssueOptions = IssueOptions
    { ioIssueNumber = Nothing
    , ioMe = False
    , ioMilestone = Nothing
    }
