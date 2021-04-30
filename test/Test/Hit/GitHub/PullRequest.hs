module Test.Hit.GitHub.PullRequest
    ( pullRequestSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldReturn)

import Hit.GitHub.PullRequest (PrTitle (..), queryPullRequests)
import Test.Hit.Data (testOwner, testRepo)

import qualified GitHub as GH


pullRequestSpec :: GH.GitHubToken -> Spec
pullRequestSpec token = describe "PullRequest" $ do
    it "doesn't find PR for non-existing branch" $
        getPRs "non-existing" `shouldReturn` Right []

    it "doesn't find closed PR with existing branch" $
        getPRs "closed-pr" `shouldReturn` Right []

    it "find open PR" $
        getPRs "chshersh/test" `shouldReturn` Right [PrTitle "Test PR"]
  where
    getPRs :: Text -> IO (Either GH.GitHubError [PrTitle])
    getPRs = queryPullRequests token testOwner testRepo
