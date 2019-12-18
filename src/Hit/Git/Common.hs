{- | Functions which can be used by serveral Hit Commands 
and are not specific to one.
-}

module Hit.Git.Common 
    (nameOrMaster
    ) where

nameOrMaster :: Maybe Text -> Text
nameOrMaster = fromMaybe "master"
