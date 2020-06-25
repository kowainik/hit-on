{- |
Module                  : Hit.Git.Clone
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit clone@ command runner and helpers.
-}

module Hit.Git.Clone
    ( runClone
    ) where

import Colourista (errorMessage)
import Shellmet ()

import Hit.Git.Common (getUsername)

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
    name <- case T.splitOn "/" txt of
        [reponame] -> getUsername >>= \u -> pure $ u <> "/" <> reponame
        [username, reponame] -> pure $ username <> "/" <> reponame
        _ -> do
            errorMessage ("Incorrect name: " <> txt <> ". Use 'repo' or 'user/repo' formats")
            exitFailure
    let gitLink = "git@github.com:" <> name <> ".git"
    "git" ["clone", gitLink]
