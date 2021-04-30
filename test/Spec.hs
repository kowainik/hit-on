module Main (main) where

import Test.Hspec (hspec)

import Hit.GitHub.Auth (getGitHubToken)

import Test.Hit.GitHub (gitHubSpec)
import Test.Hit.Names (namesSpec)
import Test.Hit.Parse (parseSpec)


main :: IO ()
main = do
    token <- getGitHubToken >>= \case
        Nothing    -> error "The env variable GITHUB_TOKEN is not set"
        Just token -> pure token

    hspec $ do
        parseSpec
        gitHubSpec token
        namesSpec
