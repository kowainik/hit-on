{- |
Module                  : Hit.Git.Wip
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Everything related to the @hit wip@ command. The command behaves
in the following way:

* If the current branch is @master@, switch to @username/wip@ branch
* Create a commit with the @WIP@ message
* Push it
-}

module Hit.Git.Wip
    ( runWip
    ) where

import Hit.Core (CommitOptions (..), ForceFlag (..))
import Hit.Git.Branch (runNew)
import Hit.Git.Commit (runCommit)
import Hit.Git.Common (getCurrentBranch)


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
