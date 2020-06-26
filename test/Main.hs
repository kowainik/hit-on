module Main (main) where

import GitHub.Data.Name (Name (..))
import Test.Hspec (describe, hspec, it, shouldBe)

import Hit.GitHub (parseOwnerRepo)


main :: IO ()
main = hspec $ do
    describe "parseOwnerRepo" $ do
        let expectedOwnerName = N "kowainik"
        let expectedRepoName = N "hit-on"
        let expectedCredentials = Just (expectedOwnerName, expectedRepoName)
        it "Parses a GitHub repo link (SSH) and gives use the Owner Name and Repo Name separately" $ do
            parseOwnerRepo "git@github.com:kowainik/hit-on.git" `shouldBe` expectedCredentials
        it "Parses a GitHub repo link (SSH) and gives use the Owner Name and Repo Name separately" $ do
            parseOwnerRepo "git@github.com:kowainik/hit-on" `shouldBe` expectedCredentials
        it "Parses a GitHub repo link (HTTPS) and gives use the Owner Name and Repo Name separately" $ do
            parseOwnerRepo "https://github.com/kowainik/hit-on.git" `shouldBe` expectedCredentials
        it "Parses a GitHub repo link (HTTPS) and gives use the Owner Name and Repo Name separately" $ do
            parseOwnerRepo "https://github.com/kowainik/hit-on" `shouldBe` expectedCredentials
        it "Parses an invalid GitHub repo link and returns a `Nothing`" $ do
            parseOwnerRepo "github.com/kowainik/hit-on" `shouldBe` Nothing
        it "Parses an invalid GitHub repo link and returns a `Nothing`" $ do
            parseOwnerRepo "https://githu.com/kowainik/hit-on" `shouldBe` Nothing
        it "Parses an invalid GitHub repo link and returns a `Nothing`" $ do
            parseOwnerRepo "https://github.com/kowainik" `shouldBe` Nothing
        it "Parses an invalid GitHub repo link and returns a `Nothing`" $ do
            parseOwnerRepo "git@github.com" `shouldBe` Nothing
