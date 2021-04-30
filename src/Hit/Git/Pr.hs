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

import Hit.Core (CommitOptions (..), ForceFlag (..), newOptionsWithName)
import Hit.Error (renderHitError)
import Hit.Git.Branch (runNew)
import Hit.Git.Commit (runCommit)
import Hit.Git.Common (getCurrentBranch, getUsername, issueFromBranch, whenOnMainBranch)
import Hit.Git.Issue (fetchIssue)
import Hit.GitHub (Issue (..), PrTitle (..), queryPullRequests, withAuthOwnerRepo)
import Hit.Hub (withHub)

import qualified Data.Text as Text


{- | @hit pr@ command.

Creates a commit and a PR if the PR for corresponding branch doesn't exist already.
It requires @hub@ tool to be installed.
-}
runPr :: Bool -> IO ()
runPr isDraft = do
    whenOnMainBranch $ runNew $ newOptionsWithName "patch"
    curBranch <- getCurrentBranch

    -- check if the open PR with head @branch_name@ already exist
    res <- withAuthOwnerRepo $ \token owner repo -> do
        queryPullRequests token owner repo curBranch

    case res of
        Left err -> do
            errorMessage "Can not get information about current PRs"
            putTextLn $ "    " <> renderHitError err
            exitFailure
        Right prs -> case prs of
            PrTitle title : _ -> do
                errorMessage $
                    "PR for the current branch already exists with the name: " <> title
                exitFailure
            [] -> do
                runCommit CommitOptions
                    { coName          = Nothing
                    , coNoIssueNumber = False
                    , coPush          = True
                    , coIsForcePush   = Simple
                    }

                -- either [] or singletonlist with comma-separated labels string
                labels <- case issueFromBranch curBranch of
                    Nothing -> pure []
                    Just n ->
                        one
                        . Text.intercalate ","
                        . issueLabels
                        <$> fetchIssue n  -- TODO: fetch less, we need only labels

                user <- getUsername
                withHub
                    $  [ "pull-request", "--no-edit", "--assign", user, "--browse" ]
                    <> [ "--draft" | isDraft ]
                    <> memptyIfTrue (null labels) ("--labels" : labels)
