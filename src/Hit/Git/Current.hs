-- | Everything related to the `hit current` command

module Hit.Git.Current
    ( runCurrent
    ) where

import Hit.ColorTerminal (arrow, greenCode, resetCode)
import Hit.Git.Common (getCurrentBranch, issueFromBranch)


{- | Part of the @hit current@ command. Prints the current branch and returns
the current issue number if possible.
-}
runCurrent :: IO (Maybe Int)
runCurrent = do
    branchName <- getCurrentBranch
    putTextLn $ arrow <> "Current branch: " <> greenCode <> branchName <> resetCode
    pure $ issueFromBranch branchName
