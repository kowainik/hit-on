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
import Shellmet ()
import System.Directory (setCurrentDirectory)

import Hit.Git.Common (getUsername)

import qualified Data.Text as Text


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
runFork name = do
    runClone name
    (_owner, repo) <- parseOwnerRepo name

    -- Step up into the folder to fork and add the upstream remote
    setCurrentDirectory $ toString repo
    "hub" ["fork"]
    successMessage $ " '" <> name <> "' repository is forked for your account at GitHub"

    "git" ["remote", "add", "upstream", "git@github.com:" <> name <> ".git"]
    successMessage "'upstream' remote is added for the repository"

    usr <- getUsername
    infoMessage $ " Link: https://github.com/" <> usr <> "/" <> repo

parseOwnerRepo :: Text -> IO (Text, Text)
parseOwnerRepo name = case Text.splitOn "/" name of
    [reponame] -> getUsername >>= \u -> pure (u, reponame)
    [username, reponame] -> pure (username, reponame)
    _other -> do
        errorMessage ("Incorrect name: " <> name <> ". Use 'repo' or 'user/repo' formats")
        exitFailure
