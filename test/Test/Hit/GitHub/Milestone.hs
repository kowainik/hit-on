module Test.Hit.GitHub.Milestone
    ( milestoneSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldReturn)

import Hit.GitHub.Milestone (Milestone (..), MilestoneNumber (..), queryLatestMilestoneNumber,
                             queryMilestoneList)
import Test.Hit.Data (testOwner, testRepo)

import qualified GitHub as GH


milestoneSpec :: GH.GitHubToken -> Spec
milestoneSpec token = describe "Milestone" $ do
    it "fetches the latest milestone" $
        queryLatestMilestoneNumber
            token
            testOwner
            testRepo
          `shouldReturn`
        Right (Just $ MilestoneNumber 2)
    it "fetches the latest milestone" $
        queryMilestoneList
            token
            testOwner
            testRepo
          `shouldReturn`
        Right testMilestones

testMilestones :: [Milestone]
testMilestones =
    [ Milestone
        { milestoneId = "MDk6TWlsZXN0b25lNjcxNDY5MQ=="
        , milestoneNumber = MilestoneNumber 2
        , milestoneTitle = "Latest milestone"
        , milestoneDescription = ""
        , milestoneProgressPercentage = 0
        , milestoneTotalIssues = 0
        }
    , Milestone
        { milestoneId = "MDk6TWlsZXN0b25lNTYzMjE1NQ=="
        , milestoneNumber = MilestoneNumber 1
        , milestoneTitle = "One big milestone"
        , milestoneDescription = "I am a milestone who is never going to be finished"
        , milestoneProgressPercentage = 33.33333333333333
        , milestoneTotalIssues = 3
        }
    ]
