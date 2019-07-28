{- | This module contains core data types used in the package.
-}

module Hit.Core
       ( PushBool (..)
       ) where


-- | Data type to represent the type of @push@: force-push or not.
data PushBool
    = Simple
    | Force
    deriving (Show, Eq)
