{-# LANGUAGE ViewPatterns #-}

-- | Logic for CLI commands to make GitHub workflows easier.

module Hit.Git
       ( runHop
       , runFresh
       , runNew
       , runPush
       , runResolve
       , runStash
       , runUnstash
       , runCommit
       , runUncommit
       , runFix
       , runAmend
       , runSync
       , runClear
       , runCurrent
       , runStatus
       , runDiff
       , runClone
       , runLog

       , getUsername
       ) where

import Hit.Git.Amend (runAmend)
import Hit.Git.Clear (runClear)
import Hit.Git.Clone (runClone)
import Hit.Git.Commit (runCommit)
import Hit.Git.Common (getUsername)
import Hit.Git.Current (runCurrent)
import Hit.Git.Diff (runDiff)
import Hit.Git.Fix (runFix)
import Hit.Git.Fresh (runFresh)
import Hit.Git.Hop (runHop)
import Hit.Git.Log (runLog)
import Hit.Git.New (runNew)
import Hit.Git.Push (runPush)
import Hit.Git.Resolve (runResolve)
import Hit.Git.Stash (runStash)
import Hit.Git.Status (runStatus)
import Hit.Git.Sync (runSync)
import Hit.Git.Uncommit (runUncommit)
import Hit.Git.Unstash (runUnstash)
