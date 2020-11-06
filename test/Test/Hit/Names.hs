module Test.Hit.Names
    ( namesSpec
    ) where

import GitHub (IssueNumber (..))
import Test.Hspec (Expectation, Spec, describe, it, runIO, shouldBe)

import Hit.Git.Branch (assignAndDisplayBranchDescription, mkBranchDescription)
import Hit.Git.Commit (toCommitMessage)
import Hit.Git.Common (getMainBranch)


namesSpec :: Spec
namesSpec = describe "Names for branches and commit messages" $ do
    branchNamesSpec
    commitMessagesSpec
    mainBranchSpec

branchNamesSpec :: Spec
branchNamesSpec = describe "Branch naming" $ do
    describe "Existing issue as argument" $ do
        it "from issue with RFC" $
            checkName Nothing "117" "117-hit-go-for-branch-switching"
        it "from issue with dots and symbols" $
            checkName Nothing "163" "163-Add-tests-commit-msgs-and"
        it "from issue with preserved special symbols" $
            checkName Nothing "175" "175-Update-header_information/-in-the-library"
    describe "Title without issue" $ do
        it "Simple title" $
            checkName Nothing "this-should-be-branch-name" "this-should-be-branch-name"
        it "Simple title with spaces" $
            checkName Nothing "this should be branch name" "this-should-be-branch-name"
        it "Simple title (more than 5 words)" $
            checkName Nothing "this-should-be-branch-name-and-not-this" "this-should-be-branch-name-and-not-this"
        it "Simple title with spaces (more than 5 words)" $
            checkName Nothing "this should be branch name and not this" "this-should-be-branch-name"
    describe "Newly created issues" $ do
        it "from IssueNumber and title" $
            checkName (Just $ IssueNumber 100000) "I am a new issue" "100000-I-am-a-new-issue"
        it "from IssueNumber and title (more than 5 words)" $
            checkName (Just $ IssueNumber 100000) "I am a new issue in here" "100000-I-am-a-new-issue"

  where
    checkName :: Maybe IssueNumber -> Text -> Text -> Expectation
    checkName mIssue title expectedName =
        assignAndDisplayBranchDescription False "no-user-name" (mkBranchDescription mIssue title) >>= \n ->
                                                                                                   n `shouldBe` expectedName

commitMessagesSpec :: Spec
commitMessagesSpec = describe "Commit messages naming" $ do
    it "with issue and 'resolve' addition" $
        toCommitMessage True "Commit message" (Just 42) `shouldBe` "[#42] Commit message\n\nResolves #42"
    it "with issue and without 'resolve' addition" $
        toCommitMessage False "Commit message" (Just 42) `shouldBe` "Commit message"
    it "without issue" $
        toCommitMessage True "Commit message" Nothing `shouldBe` "Commit message"

mainBranchSpec :: Spec
mainBranchSpec = describe "Main branch of the repo" $ do
    branch <- runIO getMainBranch
    it "should be named 'master'" $
        branch `shouldBe` "master"
