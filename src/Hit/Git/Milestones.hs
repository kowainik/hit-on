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

import Colourista (blue, cyan, errorMessage, formatWith, italic, yellow)
import Colourista.Short (b)
import Text.Printf (printf)

import Hit.Error (renderHitError)
import Hit.GitHub (Milestone (..), MilestoneNumber (..), queryMilestoneList, withAuthOwnerRepo)
import Hit.Prompt (arrow)

import qualified Data.Text as Text


{- | @hit milestones@ command.

Fetches all open milestones sorted by ID. The more recent ID would be shown the
first.
-}
runMilestones :: IO ()
runMilestones = do
    milestones <- fetchMilestones
    for_ milestones $ \milestone ->
        putTextLn $ arrow <> prettyMilestone milestone

prettyMilestone :: Milestone -> Text
prettyMilestone Milestone{..} = mconcat
    [ formatWith [blue] $ " [#" <> show (unMilestoneNumber milestoneNumber) <> "] "
    , b milestoneTitle
    , formatWith [yellow, italic] $ mconcat
        [ "  ("
        , show milestoneOpenIssues
        , "/"
        , show milestoneTotalIssues
        , ")"
        ]
    , "  "
    , formatWith [cyan] $ prettyDouble milestoneProgressPercentage
    , case Text.strip milestoneDescription of
         ""   -> ""
         desc -> "\n      " <> desc
    ]
  where
    milestoneOpenIssues :: Int
    milestoneOpenIssues = round
        $ (fromIntegral milestoneTotalIssues * milestoneProgressPercentage) / 100

fetchMilestones :: IO [Milestone]
fetchMilestones = withAuthOwnerRepo queryMilestoneList >>= \case
    Left err -> errorMessage (renderHitError err) >> exitFailure
    Right ms -> pure ms

{- | Show double prettily with only 2 digits after dot.
-}
prettyDouble :: Double -> Text
prettyDouble x =
    if fromIntegral (floor x :: Int) == x  -- display without decimal part
    then toText (printf "%.0f" x :: String) <> "%"
    else toText (printf "%.2f" x :: String) <> "%"
