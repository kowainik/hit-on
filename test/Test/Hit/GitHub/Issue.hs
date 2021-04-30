module Test.Hit.GitHub.Issue
    ( issueSpec
    ) where

import Relude.Extra.Bifunctor (secondF)
import Test.Hspec (Spec, describe, it, shouldReturn)

import Hit.Core (IssueNumber (..))
import Hit.GitHub.Issue (Issue (..), IssueTitle (..), ShortIssue (..), queryIssue, queryIssueList,
                         queryIssueTitle)
import Hit.GitHub.Milestone (MilestoneNumber (..))
import Test.Hit.Data (testOwner, testRepo)

import qualified GitHub as GH


issueSpec :: GH.GitHubToken -> Spec
issueSpec token = describe "Issue" $ do
    it "queries issue title" $
        queryIssueTitle
            token
            testOwner
            testRepo
            (IssueNumber 1)
          `shouldReturn`
        Right (IssueTitle "Issue with \"this\" and 'this'")

    it "queries issue not in milestone with multiple assignees" $
        getIssue 8 `shouldReturn` Right testIssue1

    it "queries issue in milestone with labels" $
        getIssue 10 `shouldReturn` Right testIssue2

    it "queries list of all open issues" $
        secondF (take 4) (queryIssueList token testOwner testRepo)
            `shouldReturn` Right testShortIssues

  where
    getIssue :: Int -> IO (Either GH.GitHubError Issue)
    getIssue issueNumber = queryIssue token testOwner testRepo (IssueNumber issueNumber)

testIssue1 :: Issue
testIssue1 = Issue
    { issueId = GH.Id "MDU6SXNzdWU1MzQwNDk1MjQ="
    , issueTitle = "Test 'hit new --issue'"
    , issueAuthorLogin = "chshersh"
    , issueBody = "Some description"
    , issueNumber = IssueNumber 8
    , issueUrl = "https://github.com/kowainik/hit-off/issues/8"
    , issueState = GH.open
    , issueLabels = []
    , issueAssignees = [ "chshersh", "vrom911" ]
    , issueMilestoneNumber = Nothing
    }

testIssue2 :: Issue
testIssue2 = Issue
    { issueId = GH.Id "MDU6SXNzdWU2NTMxOTQ0NTI="
    , issueTitle = "This issue should be in the milestone"
    , issueAuthorLogin = "vrom911"
    , issueBody = ""
    , issueNumber = IssueNumber 10
    , issueUrl = "https://github.com/kowainik/hit-off/issues/10"
    , issueState = GH.open
    , issueLabels = [ "documentation" ]
    , issueAssignees = [ "vrom911" ]
    , issueMilestoneNumber = Just $ MilestoneNumber 1
    }

testShortIssues :: [ShortIssue]
testShortIssues =
    [ ShortIssue
        { shortIssueNumber = IssueNumber 1
        , shortIssueTitle = "Issue with \"this\" and 'this'"
        , shortIssueAuthorLogin = "vrom911"
        , shortIssueAssignees = []
        , shortIssueMilestoneNumber = Nothing
        }
    , ShortIssue
        { shortIssueNumber = IssueNumber 8
        , shortIssueTitle = "Test 'hit new --issue'"
        , shortIssueAuthorLogin = "chshersh"
        , shortIssueAssignees = [ "chshersh", "vrom911" ]
        , shortIssueMilestoneNumber = Nothing
        }
    , ShortIssue
        { shortIssueNumber = IssueNumber 9
        , shortIssueTitle = "[RFC] Ignore RFC in issues"
        , shortIssueAuthorLogin = "vrom911"
        , shortIssueAssignees = [ ]
        , shortIssueMilestoneNumber = Just $ MilestoneNumber 1
        }
    , ShortIssue
        { shortIssueNumber = IssueNumber 10
        , shortIssueTitle = "This issue should be in the milestone"
        , shortIssueAuthorLogin = "vrom911"
        , shortIssueAssignees = [ "vrom911" ]
        , shortIssueMilestoneNumber = Just $ MilestoneNumber 1
        }
    ]
