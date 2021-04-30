module Test.Hit.GitHub.User
    ( userSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)

import Hit.GitHub.User (queryMyId)

import qualified GitHub as GH


userSpec :: GH.GitHubToken -> Spec
userSpec token = describe "User" $
    it "can query my own User ID" $
        queryMyId token >>= (`shouldSatisfy` isRight)
        -- simply checking that query works;
        -- we can't compare with any specific ID here
