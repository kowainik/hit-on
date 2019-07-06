{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GitHub.Data.Name
import Hit.Issue (parseOwnerRepo)
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "parseOwnerRepo" $ do
    let expectedOwnerName = N "kowainik"
    let expectedRepoName = N "hit-on"
    it "Parses a GitHub repo link (SSH) and gives use the Owner Name and Repo Name separately" $ do
      parseOwnerRepo "git@github.com:kowainik/hit-on.git" `shouldBe` (Just (expectedOwnerName, expectedRepoName))

    it "Parses a GitHub repo link (SSH) and gives use the Owner Name and Repo Name separately" $ do
      parseOwnerRepo "git@github.com:kowainik/hit-on" `shouldBe` (Just (expectedOwnerName, expectedRepoName))


    it "Parses a GitHub repo link (HTTPS) and gives use the Owner Name and Repo Name separately" $ do
      parseOwnerRepo "https://github.com/kowainik/hit-on.git" `shouldBe` (Just (expectedOwnerName, expectedRepoName))


    it "Parses a GitHub repo link (HTTPS) and gives use the Owner Name and Repo Name separately" $ do
      parseOwnerRepo "https://github.com/kowainik/hit-on" `shouldBe` (Just (expectedOwnerName, expectedRepoName))
