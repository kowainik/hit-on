{- |
Module                  : Hit.Git.Wip
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Everything related to the @hit wip@ command. The command behaves
in the following way:

* If the current branch is the default one (e.g. `main`), switch to
  @username/wip@ branch
* Create a commit with the @WIP@ message
* Push it
-}

module Hit.Git.Wip
    ( runWip
    ) where

import Hit.Core (CommitOptions (..), ForceFlag (..), newOptionsWithName)
import Hit.Git.Branch (runNew)
import Hit.Git.Commit (runCommit)
import Hit.Git.Common (whenOnMainBranch)


-- | @hit wip@ command: create and push @WIP@ commit.
runWip :: IO ()
runWip = do
    whenOnMainBranch $ runNew $ newOptionsWithName "wip"
    runCommit CommitOptions
        { coName          = Just "WIP"
        , coNoIssueNumber = True
        , coPush          = True
        , coIsForcePush   = Simple
        }
