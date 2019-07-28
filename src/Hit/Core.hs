{- | This module contains core data types used in the package.
-}

module Hit.Core
       ( PushBool (..)
       , CommitOptions (..)
       ) where


-- | Data type to represent the type of @push@: force-push or not.
data PushBool
    = Simple
    | Force
    deriving (Show, Eq)

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
