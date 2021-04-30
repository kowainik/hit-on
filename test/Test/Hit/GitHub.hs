module Test.Hit.GitHub
    ( gitHubSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Hit.GitHub.Issue (issueSpec)
import Test.Hit.GitHub.Milestone (milestoneSpec)
import Test.Hit.GitHub.PullRequest (pullRequestSpec)
import Test.Hit.GitHub.User (userSpec)

import qualified GitHub as GH


gitHubSpec :: GH.GitHubToken -> Spec
gitHubSpec token = describe "GitHub API" $ do
    userSpec token
    pullRequestSpec token
    milestoneSpec token
    issueSpec token
