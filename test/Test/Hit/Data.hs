module Test.Hit.Data
    ( testOwner
    , testRepo
    ) where

import Hit.Core (Owner (..), Repo (..))


testOwner :: Owner
testOwner = Owner "kowainik"

testRepo :: Repo
testRepo = Repo "hit-off"
