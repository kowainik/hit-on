{- |
Module                  : Hit.Git.Milestones
Copyright               : (c) 2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@hit milestones@ command runner and helpers.
-}

module Hit.Git.Milestones
    ( runMilestones
    ) where

import Colourista (blue, errorMessage, formatWith, italic, yellow)
import Colourista.Short (b)
import GitHub (Milestone (..), untagId)
import GitHub.Endpoints.Issues.Milestones (milestones')

import Hit.GitHub (withOwnerRepo)
import Hit.Prompt (arrow)

import qualified Data.Text as T


{- | @hit milestones@ command.

Fetches all open milestones sorted by ID. The more recent ID would be shown the
first.
-}
runMilestones :: IO ()
runMilestones = withOwnerRepo milestones' >>= \case
    Left err -> do
        errorMessage ("Could not fetch the milestones\n    " <> show err)
        exitFailure
    Right ms -> for_ (sortWith (Down . untagId . milestoneNumber) $ toList ms) $ \m ->

        putTextLn $ arrow <> prettyMilestone m

prettyMilestone :: Milestone -> Text
prettyMilestone Milestone{..} =
    formatWith [blue] (" [#" <> show (untagId milestoneNumber) <> "] ")
    <> b milestoneTitle
    <> formatWith [yellow, italic] ("  (" <> show milestoneOpenIssues <> "/" <> show (milestoneOpenIssues + milestoneClosedIssues) <> ")")
    <> case T.strip <$> milestoneDescription of
         Just ""   -> ""
         Just desc -> "\n      " <> desc
         Nothing   -> ""
