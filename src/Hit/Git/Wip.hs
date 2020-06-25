{- | Everything related to the @hit wip@ command. The command behaves
in the following way:

* If the current branch is @master@, switch to @username/wip@ branch
* Create a commit with the @WIP@ message
* Push it
-}

module Hit.Git.Wip
    ( runWip
    ) where

import Hit.Core (CommitOptions (..), PushBool (..))
import Hit.Git.Commit (runCommit)
import Hit.Git.Common (getCurrentBranch)
import Hit.Git.New (runNew)


-- | @hit wip@ command: create and push @WIP@ commit.
runWip :: IO ()
runWip = do
    curBranch <- getCurrentBranch
    when (curBranch == "master") $ runNew False "wip"
    runCommit CommitOptions
        { coName          = Just "WIP"
        , coNoIssueNumber = True
        , coPush          = True
        , coIsForcePush   = Simple
        }
