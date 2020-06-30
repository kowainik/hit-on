{- |
Module                  : Hit.Git.Clone
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit clone@ and @hit fork@ command runners and helpers.
-}

module Hit.Git.Clone
    ( runClone
    , runFork
    ) where

import Colourista (errorMessage, infoMessage, successMessage)
import GitHub.Endpoints.Repos (forkExistingRepo')
import Shellmet ()

import Hit.Git.Common (getUsername)
import Hit.GitHub (getGitHubToken, makeName)

import qualified Data.Text as T


{- | @hit clone@ command receives the name of the repo in the following
formats:

* @reponame@ — current user's username is used to clone the repo from.
* @name/reponame@ — specified GitHub username is used to clone the repo from.

__Note__ that the @ssh@ strategy is used for cloning from GitHub. See the corresponding @git@ command:

@
git clone git@github.com:username/project-name.git
@
-}
runClone :: Text -> IO ()
runClone txt = do
    (owner, repo) <- parseOwnerRepo txt
    let name = owner <> "/" <> repo
    let gitLink = "git@github.com:" <> name <> ".git"
    "git" ["clone", gitLink]
    infoMessage $ " '" <> name <> "' repository is cloned to the '" <> repo <>"/' folder."

{- |
-}
runFork :: Text -> IO ()
runFork name = getGitHubToken >>= \case
    Nothing -> errorMessage "Can not get GITHUB_TOKEN" >> exitFailure
    Just auth -> do
        (owner, repo) <- parseOwnerRepo name
        forkExistingRepo' auth (makeName owner) (makeName repo) Nothing >>= \case
            Left err -> do
                errorMessage $ "Can not fork the repository: " <> name
                errorMessage $ show err
                exitFailure
            Right _ -> do
                successMessage $ " '" <> name <> "' repository is forked for your account at GitHub"
                usr <- getUsername
                infoMessage $ " Link: https://github.com/" <> usr <> "/" <> repo

                runClone repo

parseOwnerRepo :: Text -> IO (Text, Text)
parseOwnerRepo name = case T.splitOn "/" name of
    [reponame] -> getUsername >>= \u -> pure (u, reponame)
    [username, reponame] -> pure (username, reponame)
    _ -> do
        errorMessage ("Incorrect name: " <> name <> ". Use 'repo' or 'user/repo' formats")
        exitFailure
