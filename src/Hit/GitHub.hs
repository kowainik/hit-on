{- |
Module                  : Hit.GitHub
Copyright               : (c) 2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains helper functions to work with GitHub API.
-}

module Hit.GitHub
    ( withOwnerRepo
    , withAuthOwnerRepo

    , makeName
    , getGitHubToken

      -- * GitHub URLs
    , getOwnerRepo
    , parseOwnerRepo
    ) where

import Colourista (errorMessage)
import GitHub (Error (..), Name, Owner, Repo, mkName)
import GitHub.Auth (Auth (OAuth))
import Shellmet (($|))
import System.Environment (lookupEnv)

import qualified Data.Text as T


-- | Perform action by given auth token, owner and repo name.
withOwnerRepo
    :: (Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error a))
    -> IO (Either Error a)
withOwnerRepo action = getOwnerRepo >>= \case
    Just (owner, repo) -> do
        token <- getGitHubToken
        action token owner repo
    Nothing -> do
        let errorText = "Cannot get the owner/repo names"
        errorMessage errorText
        pure $ Left $ ParseError errorText

{- | Similar to 'withOwnerRepo', but returns the 'UserError' when cannot get the
GitHub Token, as the given action should work with the 'Auth' instead of 'Maybe
Auth'.
-}
withAuthOwnerRepo
    :: (Auth -> Name Owner -> Name Repo -> IO (Either Error a))
    -> IO (Either Error a)
withAuthOwnerRepo action = withOwnerRepo $ \token owner repo -> case token of
    Just auth -> action auth owner repo
    Nothing -> do
        let errorText = "Can not get GITHUB_TOKEN"
        errorMessage errorText
        pure $ Left $ UserError errorText

-- | Smart constructor for 'Name'.
makeName :: forall a . Text -> Name a
makeName = mkName (Proxy @a)

-- | Get authentication GitHub token from the environment variable @GITHUB_TOKEN@.
getGitHubToken :: IO (Maybe Auth)
getGitHubToken = do
    token <- lookupEnv "GITHUB_TOKEN"
    pure $ OAuth . encodeUtf8 <$> token

----------------------------------------------------------------------------
-- Fetch and parse name and repo from URL
----------------------------------------------------------------------------

-- | Get the owner and the repository name.
getOwnerRepo :: IO (Maybe (Name Owner, Name Repo))
getOwnerRepo = parseOwnerRepo <$> "git" $| ["remote", "get-url", "origin"]

{- |
__Note:__ this works with GitHub projects!

This function supports four kinds of the URLs:

SSH one:

@
git@github.com:kowainik/hit-on.git
@

or

@
git@github.com:kowainik/hit-on
@

And HTTPS one:

@
https://github.com/kowainik/hit-on.git
@

or

@
https://github.com/kowainik/hit-on
@
-}
parseOwnerRepo :: Text -> Maybe (Name Owner, Name Repo)
parseOwnerRepo url =
    ( T.stripPrefix "git@github.com:"     url
  <|> T.stripPrefix "https://github.com/" url
    ) >>= stripGitSuffix >>= separateName
  where
    separateName :: Text -> Maybe (Name Owner, Name Repo)
    separateName nm =
        let (owner, T.drop 1 -> repo) = T.breakOn "/" nm in
        guard (owner /= "" && repo /= "") *> Just (makeName owner, makeName repo)

    stripGitSuffix :: Text -> Maybe Text
    stripGitSuffix x = whenNothing (T.stripSuffix ".git" x) (Just x)
