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
    ) where

import Colourista (blue, blueBg, bold, errorMessage, formatWith, green, red, reset, skipMessage,
                   successMessage, warningMessage)
import Data.Aeson (Array, FromJSON (..), withObject, (.:))
import Data.Aeson.Types (Parser)
import Prolens (set)

import Hit.Core (IssueOptions (..), MilestoneOption (..), Owner (..), Repo (..))
import Hit.Error (renderHitError)
import Hit.Git.Common (getUsername)
import Hit.GitHub.Auth (withAuthOwnerRepo)
import Hit.GitHub.Issue (Issue (..), ShortIssue (..), issueToShort, queryIssue, queryIssueList)
import Hit.GitHub.Milestone (MilestoneNumber (..), queryLatestMilestoneNumber)
import Hit.Prompt (arrow)

import qualified Hit.Formatting as Fmt

import qualified Data.Text as Text
import qualified GitHub as GH


-- | Run the @issue@ command.
runIssue :: IssueOptions -> IO ()
runIssue IssueOptions{..} = case ioIssueNumber of
    Just num -> getIssue num
    Nothing  -> meToUsername ioMe >>= printFilteredIssues ioMilestone

----------------------------------------------------------------------------
-- Single issue processing
----------------------------------------------------------------------------

-- | Get the 'Issue' by given issue number and pretty print it fully to terminal.
getIssue :: Int -> IO ()
getIssue num = fetchIssue num >>= putTextLn . showIssueFull

{- | Fetch 'Issue' by number. If no issue found then print error and
exit with failure.
-}
fetchIssue :: Int -> IO Issue
fetchIssue iNum = withAuthOwnerRepo (\t o r -> queryIssue t o r iNum) >>= \case
    Left err    -> errorMessage (renderHitError err) >> exitFailure
    Right issue -> pure issue

{- | Fetch 'IssueTitle' by number. If no issue found then print error and
exit with failure.
-}
fetchIssueTitle :: Int -> IO Text
fetchIssueTitle iNum = withAuthOwnerRepo (\t o r -> queryIssueTitle t o r iNum) >>= \case
    Left err         -> errorMessage (renderHitError err) >> exitFailure
    Right issueTitle -> pure $ unIssueTitle issueTittle

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
        pure $ filterIssues milestoneNumber issues
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
printIssues issues = let maxLen = Fmt.maxLenOn showIssueNumber issues in
    if null issues
    then skipMessage "There are no open issues satisfying the provided filters"
    else for_ issues $ \issue@ShortIssue{..} -> do
        let thisLen = Text.length $ show shortIssueNumber
            padSize = maxLen - thisLen
        putTextLn $ showShortIssue blue padSize issue

-- | Show issue number with alignment and its name.
showShortIssue :: Text -> Int -> ShortIssue -> Text
showShortIssue colorCode padSize ShortIssue{..} = mconcat
    [ arrow
    , colorCode
    , " [#" <> show shortIssueNumber <> "] "
    , spaces padSize
    , reset
    , shortIssueTitle
    ]

-- | Show full information about the issue.
showIssue :: Issue -> Text
showIssue i@Issue{..} = T.intercalate "\n" $
       showShortIssue (statusToCode issueState) 0 (showShortIssue $ issueToShort i)
     : [ highlight "    Assignees: " <> assignees | not $ null issueAssignees]
    ++ [ highlight "    Labels: " <> labels | not $ null issueLabels]
    ++ [ highlight "    URL: " <> getUrl url | Just url <- [issueHtmlUrl]]
    ++ [ indentDesc desc | Just (T.strip -> desc) <- [issueBody], desc /= ""]
  where
    statusToCode :: GH.IssueState -> Text
    statusToCode = \case
        IssueOpen   -> blue
        IssueClosed -> red

    indentDesc :: Text -> Text
    indentDesc = unlines
        . map ("    " <> )
        . (highlight "Description:" :)
        . lines

    assignees :: Text
    assignees = T.intercalate ", " $ map (untagName . simpleUserLogin) issueAssignees

    labels :: Text
    labels = T.intercalate " " $ map (putLabel . untagName . labelName) issueLabels

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
    Just CurrentMilestone    -> fetchCurrentMilestoneId
    Nothing                  -> pure Nothing

{- | Fetches all open milestones. Then figure out the current one and return its
ID as 'Int'.

If it could not fetch, or there is no open milestones then prints a warning
message and returns 'Nothing'.
-}
fetchCurrentMilestoneId :: IO (Maybe MilestoneNumber)
fetchCurrentMilestoneId = withAuthOwnerRepo queryLatestMilestoneNumber >>= \case
    Left err -> Nothing <$ warningMessage ("Could not fetch the milestones\n    " <> show err)
    Right ms -> case ms of
        Nothing -> Nothing <$ warningMessage "There are no open milestones for this project"
        Just m  -> pure $ Just m
