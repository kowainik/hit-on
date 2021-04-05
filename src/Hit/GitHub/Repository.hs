{- |
Module                  : Hit.GitHub.Repository
Copyright               : (c) 2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Repository-related queries and data types.
-}

module Hit.GitHub.Repository
    ( RepositoryField (..)
    , RepositoryNodes (..)
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))


{- | Helper type to parse nodes of the top-level @repository@ query.

The JSON usually has the following shape:

@
{
  "data": {
    "repository": {
      "<name>": {
        "nodes": [
            ...
@
-}
newtype RepositoryNodes (name :: Symbol) a = RepositoryNode
    { unRepositoryNodes :: [a]
    }

instance
    (KnownSymbol name, FromJSON a, Typeable a)
    => FromJSON (RepositoryNode name a)
  where
    parseJSON = withObject ("RepositoryNode " <> typeName @a) $ \o -> do
        repository <- o .: "repository"
        let itemName = symbolVal (Proxy @name)
        items <- repository .: itemName
        nodes <- items .: "nodes"
        RepositoryNode <$> mapM parseJSON nodes

{- | Helper type to parse a given field of the top-level @repository@ query.

The JSON usually has the following shape:

@
{
  "data": {
    "repository": {
      "<name>": {
            ...
@
-}
newtype RepositoryField (name :: Symbol) a = RepositoryField
    { unRepositoryField :: [a]
    }

instance
    (KnownSymbol name, FromJSON a, Typeable a)
    => FromJSON (RepositoryField name a)
  where
    parseJSON = withObject ("RepositoryField " <> typeName @a) $ \o -> do
        repository <- o .: "repository"
        let fieldName = symbolVal (Proxy @name)
        RepositoryField <$> (repository .: fieldName)
