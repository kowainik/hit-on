{- |
Module                  : Hit.Git.Pr
Copyright               : (c) 2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit pr@ command runner and helpers.
-}

module Hit.Git.Pr
    ( runPr
    ) where

import Colourista (errorMessage)
import GitHub (untagName)
import GitHub.Data.Options (optionsHead)
import GitHub.Data.Request (FetchCount (..))
import GitHub.Endpoints.PullRequests (pullRequestsForR)
import GitHub.Request (executeRequest)

import Hit.Core (CommitOptions (..), ForceFlag (..))
import Hit.Git.Commit (runCommit)
import Hit.Git.Common (getCurrentBranch, getUsername)
import Hit.Git.New (runNew)
import Hit.GitHub (withAuthOwnerRepo)
import Hit.Hub (withHub)

import qualified Data.Vector as V


{- | @hit pr@ command.

Creates a commit and a PR if the PR for corresponding branch doesn't exist already.
It requires @hub@ tool to be installed.
-}
runPr :: Bool -> IO ()
runPr isDraft = do
    whenM ((== "master") <$> getCurrentBranch) $ runNew False "patch"
    curBranch <- getCurrentBranch
    -- check if the open PR with head @owner:branch_name@ already exist
    res <- withAuthOwnerRepo $ \auth owner repo -> do
        let headPrMod = optionsHead $ untagName owner <> ":" <> curBranch
        executeRequest auth (pullRequestsForR owner repo headPrMod FetchAll)
    case res of
        Left err -> do
            errorMessage "Can not get information about current PRs"
            putTextLn $ "    " <> show err
            exitFailure
        Right prs ->
            if (not $ V.null prs)
            then errorMessage "PR for the current branch already exists" >> exitFailure
            else do
                runCommit CommitOptions
                    { coName          = Nothing
                    , coNoIssueNumber = False
                    , coPush          = True
                    , coIsForcePush   = Simple
                    }
                user <- getUsername
                withHub $ ["pull-request", "--no-edit", "--assign", user, "--browse"]
                    <> ["--draft" | isDraft]
