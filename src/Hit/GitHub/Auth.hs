{- |
Module                  : Hit.GitHub.Auth
Copyright               : (c) 2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Functions to perform authenticated GitHub API requests.
-}

module Hit.GitHub.Auth
    ( withAuthOwnerRepo
    ) where

import Shellmet (($|))

import Hit.Core (Owner, Repo)
import Hit.Error (HitError (..))

import qualified Data.Text as Text
import qualified GitHub as GH


{- | Perform action by given 'GH.GitHubToken' , 'Owner' and 'Repo.

All actions to query GraphQL GitHub API require authentication token.
-}
withAuthOwnerRepo
    :: (GH.GitHubToken -> Owner -> Repo -> IO a)
    -> IO (Either HitError a)
withAuthOwnerRepo action = GH.getGitHubToken "GITHUB_TOKEN" >>= \case
    Nothing    -> pure $ Left NoGitHubTokenEnv
    Just token -> getOwnerRepo >>= \case
        Nothing            -> pure $ Left InvalidOwnerRepo
        Just (owner, repo) -> action token owner repo

-- withOwnerRepo
--     :: (Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error a))
--     -> IO (Either Error a)
-- withOwnerRepo action = getOwnerRepo >>= \case
--     Just (owner, repo) -> do
--         token <- getGitHubToken
--         action token owner repo
--     Nothing -> do
--         let errorText = "Cannot get the owner/repo names"
--         errorMessage errorText
--         pure $ Left $ ParseError errorText

----------------------------------------------------------------------------
-- Fetch and parse name and repo from URL
----------------------------------------------------------------------------

-- | Get the owner and the repository name.
getOwnerRepo :: IO (Maybe (Owner, Repo))
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
parseOwnerRepo :: Text -> Maybe (Owner, Repo)
parseOwnerRepo url =
    ( Text.stripPrefix "git@github.com:"     url
  <|> Text.stripPrefix "https://github.com/" url
    ) >>= stripGitSuffix >>= separateName
  where
    separateName :: Text -> Maybe (Owner, Repo)
    separateName nm =
        let (owner, Text.drop 1 -> repo) = Text.breakOn "/" nm in
        guard (owner /= "" && repo /= "") $> (Owner owner, Repo repo)

    stripGitSuffix :: Text -> Maybe Text
    stripGitSuffix x = whenNothing (T.stripSuffix ".git" x) (Just x)
