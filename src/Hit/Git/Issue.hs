{- |
Module                  : Hit.GitHub.Issue
Copyright               : (c) 2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Issue-related queries and data types.
-}

module Hit.Git.Issue
    ( runIssue

      -- * Helpers
    , fetchIssue
    , fetchIssueTitle
    , getAllIssues
    , getMilestoneNumber
    , printIssues
    , showIssueNumber
    ) where

import Colourista (blue, blueBg, bold, errorMessage, formatWith, green, red, reset, skipMessage,
                   warningMessage)

import Hit.Core (IssueNumber (..), IssueOptions (..), MilestoneOption (..))
import Hit.Error (renderHitError)
import Hit.Git.Common (meToUsername)
import Hit.GitHub.Auth (withAuthOwnerRepo)
import Hit.GitHub.Issue (Issue (..), IssueTitle (..), ShortIssue (..), issueToShort, queryIssue,
                         queryIssueList, queryIssueTitle)
import Hit.GitHub.Milestone (MilestoneNumber (..), queryLatestMilestoneNumber)
import Hit.Prompt (arrow)

import qualified Data.Text as Text
import qualified GitHub as GH

import qualified Hit.Formatting as Fmt


-- | Run the @issue@ command.
runIssue :: IssueOptions -> IO ()
runIssue IssueOptions{..} = case ioIssueNumber of
    Just num -> getIssue num
    Nothing  -> meToUsername ioMe >>= printFilteredIssues ioMilestone

----------------------------------------------------------------------------
-- Single issue processing
----------------------------------------------------------------------------

-- | Get the 'Issue' by given issue number and pretty print it fully to terminal.
getIssue :: IssueNumber -> IO ()
getIssue num = fetchIssue num >>= putTextLn . showIssue

{- | Fetch 'Issue' by number. If no issue found then print error and
exit with failure.
-}
fetchIssue :: IssueNumber -> IO Issue
fetchIssue iNum = withAuthOwnerRepo (\t o r -> queryIssue t o r iNum) >>= \case
    Left err    -> errorMessage (renderHitError err) >> exitFailure
    Right issue -> pure issue

{- | Fetch 'IssueTitle' by number. If no issue found then print error and
exit with failure.
-}
-- TODO: reduce duplication with 'fetchIssue'
fetchIssueTitle :: IssueNumber -> IO Text
fetchIssueTitle iNum = withAuthOwnerRepo (\t o r -> queryIssueTitle t o r iNum) >>= \case
    Left err         -> errorMessage (renderHitError err) >> exitFailure
    Right issueTitle -> pure $ unIssueTitle issueTitle

----------------------------------------------------------------------------
-- Multiple list processing
----------------------------------------------------------------------------

{- | Outputs the list of the open issues for the current project
with applied filters.

See 'getAllIssues' to find out more about filtering.
-}
printFilteredIssues
    :: Maybe MilestoneOption  -- ^ Project Milestone
    -> Maybe Text  -- ^ User name of the assignee
    -> IO ()
printFilteredIssues milestone me = getAllIssues milestone me >>= printIssues

{- | Get the list of the opened issues for the current project
filtered out by the given input:

  * Only current user's issues?
  * Only issues from the current/specified milestone?
-}
getAllIssues
    :: Maybe MilestoneOption  -- ^ Project Milestone
    -> Maybe Text  -- ^ User name of the assignee
    -> IO [ShortIssue]
getAllIssues milestoneOpt me = withAuthOwnerRepo queryIssueList >>= \case
    Left err -> errorMessage (renderHitError err) >> exitFailure
    Right issues -> do
        milestoneNum <- getMilestoneNumber milestoneOpt
        pure $ filterIssues milestoneNum issues
  where
    filterIssues :: Maybe MilestoneNumber -> [ShortIssue] -> [ShortIssue]
    filterIssues milestoneNumber =
        filter (\i -> my i && i `isInMilestone` milestoneNumber)

    my :: ShortIssue -> Bool
    my issue = case me of
        Just username -> elem username (shortIssueAssignees issue)
        Nothing       -> True

    isInMilestone :: ShortIssue -> Maybe MilestoneNumber -> Bool
    isInMilestone ShortIssue{..} = \case
        Just milestoneNum -> shortIssueMilestoneNumber == Just milestoneNum
        Nothing           -> True

----------------------------------------------------------------------------
-- Issue formatting
----------------------------------------------------------------------------

{- | Outputs the list of the given issues for the current project.
-}
printIssues :: [ShortIssue] -> IO ()
printIssues issues = let maxLen = Fmt.maxLenOn (showIssueNumber . shortIssueNumber) issues in
    if null issues
    then skipMessage "There are no open issues satisfying the provided filters"
    else for_ issues $ \issue@ShortIssue{..} -> do
        let thisLen = Text.length $ show $ unIssueNumber shortIssueNumber
            padSize = maxLen - thisLen
        putTextLn $ showShortIssue blue padSize issue

showIssueNumber :: IssueNumber -> Text
showIssueNumber (IssueNumber issueNumber) = " [#" <> show issueNumber <> "] "

-- | Show issue number with alignment and its name.
showShortIssue :: Text -> Int -> ShortIssue -> Text
showShortIssue colorCode padSize ShortIssue{..} = mconcat
    [ arrow
    , colorCode
    , showIssueNumber shortIssueNumber
    , Fmt.spaces padSize
    , reset
    , shortIssueTitle
    ]

-- | Show full information about the issue.
showIssue :: Issue -> Text
showIssue i@Issue{..} = Text.intercalate "\n" $
       showShortIssue (statusToCode issueState) 0 (issueToShort i)
     : [ highlight "    Assignees: " <> assignees | not $ null issueAssignees]
    ++ [ highlight "    Labels: " <> labels | not $ null issueLabels]
    ++ [ highlight "    URL: " <> issueUrl]
    ++ [ indentDesc desc | desc <- [Text.strip issueBody], desc /= ""]
  where
    statusToCode :: GH.IssueState -> Text
    statusToCode = \case
        GH.IssueOpen   -> blue
        GH.IssueClosed -> red

    indentDesc :: Text -> Text
    indentDesc = unlines
        . map ("    " <> )
        . (highlight "Description:" :)
        . lines

    assignees :: Text
    assignees = Text.intercalate ", " issueAssignees

    labels :: Text
    labels = Text.intercalate " " $ map putLabel issueLabels

    putLabel :: Text -> Text
    putLabel = formatWith [blueBg]

    highlight :: Text -> Text
    highlight = formatWith [bold, green]

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

{- | From the given 'MilestoneOption' type try to get the milestone
number.
-}
getMilestoneNumber :: Maybe MilestoneOption -> IO (Maybe MilestoneNumber)
getMilestoneNumber = \case
    Just (MilestoneNum mNum) -> pure $ Just $ MilestoneNumber mNum
    Just CurrentMilestone    -> fetchCurrentMilestoneNumber
    Nothing                  -> pure Nothing

{- | Fetches the latest milestone number.

If it could not fetch, or there is no open milestones then prints a warning
message and returns 'Nothing'.
-}
fetchCurrentMilestoneNumber :: IO (Maybe MilestoneNumber)
fetchCurrentMilestoneNumber = withAuthOwnerRepo queryLatestMilestoneNumber >>= \case
    Left err -> Nothing
        <$ warningMessage ("Could not fetch the milestones\n    "
        <> renderHitError err)
    Right ms -> case ms of
        Nothing -> Nothing <$ warningMessage "There are no open milestones for this project"
        Just m  -> pure $ Just m
