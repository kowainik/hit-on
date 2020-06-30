{- |
Module                  : Hit.Issue
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains functions to work with issues withing GitHub API.
-}

module Hit.Issue
    ( -- * For CLI commands
      runIssue
    , createIssue
    , assignIssue
    , fetchIssue

      -- * Issues helpers
    , getAllIssues
    , printIssues

      -- * Internal helpers
    , meToUsername
    , mkIssueId
    , getIssueTitle
    , showIssueName
    ) where

import Colourista (blue, blueBg, bold, errorMessage, formatWith, green, red, reset, skipMessage,
                   successMessage, warningMessage)
import Data.Vector (Vector)
import GitHub (Error (..), Id, Issue (..), IssueLabel (..), IssueState (..), Name, SimpleUser (..),
               User, getUrl, milestoneNumber, mkId, unIssueNumber, untagId, untagName)
import GitHub.Data.Options (stateOpen)
import GitHub.Endpoints.Issues (EditIssue (..), NewIssue (..), editOfIssue, issue', issuesForRepo')
import GitHub.Endpoints.Issues.Milestones (milestones')

import Hit.Core (IssueOptions (..), Milestone (..))
import Hit.Git.Common (getUsername)
import Hit.GitHub (makeName, withAuthOwnerRepo, withOwnerRepo)
import Hit.Prompt (arrow)

import qualified Hit.Formatting as Fmt

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GitHub.Endpoints.Issues as GitHub


----------------------------------------------------------------------------
-- CLI for issues
----------------------------------------------------------------------------

-- | Run the @issue@ command.
runIssue :: IssueOptions -> IO ()
runIssue IssueOptions{..} = case ioIssueNumber of
    Just num -> getIssue $ mkIssueId num
    Nothing  -> meToUsername ioMe >>= printFilteredIssues ioMilestone

{- | If requested, get the username.
-}
meToUsername :: Bool -> IO (Maybe Text)
meToUsername isMe =
    if isMe
    then Just <$> getUsername
    else pure Nothing

{- | Outputs the list of the open issues for the current project
with applied filters.

See 'getAllIssues' to find out more about filtering.
-}
printFilteredIssues
    :: Maybe Milestone  -- ^ Project Milestone
    -> Maybe Text  -- ^ User name of the assignee
    -> IO ()
printFilteredIssues milestone me = getAllIssues milestone me >>= printIssues

{- | Outputs the list of the given issues for the current project.
-}
printIssues :: Vector Issue -> IO ()
printIssues issues = let maxLen = Fmt.maxLenOn showIssueNumber issues in
    if V.null issues
    then skipMessage "There are no open issues satisfying the provided filters"
    else for_ issues $ \i -> do
        let thisLen = T.length $ showIssueNumber i
            padSize = maxLen - thisLen
        putTextLn $ showIssueName blue padSize i

{- | Get the list of the opened issues for the current project
filtered out by the given input:
  * Only current user's issues?
  * Only issues from the current/specified milestone?
-}
getAllIssues
    :: Maybe Milestone  -- ^ Project Milestone
    -> Maybe Text  -- ^ User name of the assignee
    -> IO (Vector Issue)
getAllIssues milestone me = withOwnerRepo (\t o r -> issuesForRepo' t o r stateOpen) >>= \case
    Left err -> errorMessage (show err) >> exitFailure
    Right is -> do
        milestoneId <- getMilestoneId
        pure $ filterIssues milestoneId is
  where
    filterIssues :: Maybe Int -> Vector Issue -> Vector Issue
    filterIssues milestoneId = V.filter
        (\i ->
            isNotPR i
            && my i
            && i `isInMilestone` milestoneId
        )

    my :: Issue -> Bool
    my issue = case me of
        Just (makeName -> username) -> username `isAssignedToIssue` issue
        Nothing                     -> True

    isNotPR :: Issue -> Bool
    isNotPR Issue{..} = isNothing issuePullRequest

    getMilestoneId :: IO (Maybe Int)
    getMilestoneId = case milestone of
        Just (MilestoneId mId) -> pure $ Just mId
        Just CurrentMilestone  -> fetchCurrentMilestoneId
        Nothing                -> pure Nothing

    isInMilestone :: Issue -> Maybe Int -> Bool
    isInMilestone Issue{..} = \case
        Just milestoneId -> issueMilestoneId == Just milestoneId
        Nothing  -> True
      where
        issueMilestoneId :: Maybe Int
        issueMilestoneId = untagId . milestoneNumber <$> issueMilestone

-- | Show issue number with alignment and its name.
showIssueName :: Text -> Int -> Issue -> Text
showIssueName colorCode padSize i@Issue{..} =
    arrow <> colorCode <> " [#" <> showIssueNumber i <> "] " <> padding <> reset <> issueTitle
  where
    padding :: Text
    padding = T.replicate padSize " "

-- | Show the issue number.
showIssueNumber :: Issue -> Text
showIssueNumber = show . unIssueNumber . issueNumber

-- | Get the 'Issue' by given issue number and pretty print it fully to terminal.
getIssue :: Id Issue -> IO ()
getIssue num = fetchIssue num >>= putTextLn . showIssueFull

-- | Show full information about the issue.
showIssueFull :: Issue -> Text
showIssueFull i@Issue{..} = T.intercalate "\n" $
       showIssueName (statusToCode issueState) 0 i
     : [ highlight "    Assignees: " <> assignees | not $ null issueAssignees]
    ++ [ highlight "    Labels: " <> labels | not $ null issueLabels]
    ++ [ highlight "    URL: " <> getUrl url | Just url <- [issueHtmlUrl]]
    ++ [ indentDesc desc | Just (T.strip -> desc) <- [issueBody], desc /= ""]
  where
    statusToCode :: IssueState -> Text
    statusToCode = \case
        StateOpen -> blue
        StateClosed -> red

    indentDesc :: Text -> Text
    indentDesc = unlines
        . map ("    " <> )
        . (highlight "Description:" :)
        . lines

    assignees :: Text
    assignees = T.intercalate ", "
        $ map (untagName . simpleUserLogin)
        $ toList issueAssignees

    labels :: Text
    labels = T.intercalate " "
        $ map (putLabel . untagName . labelName)
        $ toList issueLabels

    putLabel :: Text -> Text
    putLabel = formatWith [blueBg]

    highlight :: Text -> Text
    highlight = formatWith [bold, green]

-- | Create an 'Issue' by given title 'Text'
-- QUESTION: should we create 'Login' newtype to add more type-safety here?
createIssue :: Text -> Text -> IO (Either Error Issue)
createIssue title login = withAuthOwnerRepo $ \token owner repo ->
    GitHub.createIssue token owner repo $ mkNewIssue title login

{- | Assign the user to the given 'Issue'.

This function can fail assignment due to the following reasons:

 * Auth token fetch failure
 * Assignment query to GutHub failure

The function should inform user about corresponding 'Error' in each case and
continue working.
-}
assignIssue :: Issue -> Text -> IO ()
assignIssue issue username = do
    res <- withAuthOwnerRepo $ \token owner repo -> do
        let assignee :: Name User
            assignee = makeName @User username
        let curAssignees :: V.Vector (Name User)
            curAssignees = V.map simpleUserLogin $ issueAssignees issue

        if assignee `isAssignedToIssue` issue
        then pure $ Right (issue, True)
        else do
            -- TODO: this is hack to cheat on GitHub library, as it
            -- doesn't use the correct id in query.
            let issId = mkIssueId (unIssueNumber $ issueNumber issue)
            (, False) <<$>> GitHub.editIssue token owner repo issId editOfIssue
                { editIssueAssignees = Just $ V.cons assignee curAssignees
                }

    case res of
        Right (iss, isAlreadyAssigned) ->
            if isAlreadyAssigned
            then pass
            else successMessage $ "You were assigned to the issue #" <>
                showIssueNumber iss
        Left err  -> do
            errorMessage "Can not assign you to the issue."
            putTextLn $ "    " <> show err

-- | Is the user assigned to the given 'Issue'?
isAssignedToIssue :: Name User -> Issue -> Bool
isAssignedToIssue assignee = V.elem assignee .
    V.map simpleUserLogin . issueAssignees

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- | Fetch only issue title.
getIssueTitle :: Id Issue -> IO Text
getIssueTitle num = issueTitle <$> fetchIssue num

{- | Fetch 'Issue' by 'Id'. If no issue found then print error and
exit with failure.
-}
fetchIssue :: Id Issue -> IO Issue
fetchIssue iNum = withOwnerRepo (\t o r -> issue' t o r iNum) >>= \case
    Left err -> errorMessage (show err) >> exitFailure
    Right issue -> pure issue

{- | Fetches all open milestones. Then figure out the current one and return its
ID as 'Int'.

If it could not fetch, or there is no open milestones then prints a warning
message and returns 'Nothing'.
-}
fetchCurrentMilestoneId :: IO (Maybe Int)
fetchCurrentMilestoneId = withOwnerRepo milestones' >>= \case
    Left err -> Nothing <$ warningMessage ("Could not fetch the milestones\n    " <> show err)
    Right ms -> case sortWith Down $ map (untagId . milestoneNumber) $ toList ms of
        []  -> warningMessage "There are no open milestones for this project" >> pure Nothing
        m:_ -> pure $ Just m

-- | Smart constructor for @'Id' 'Issue'@.
mkIssueId :: Int -> Id Issue
mkIssueId = mkId $ Proxy @Issue

-- | Create new issue with title and assignee.
mkNewIssue :: Text -> Text -> NewIssue
mkNewIssue title login = NewIssue
    { newIssueTitle     = title
    , newIssueBody      = Nothing
    , newIssueAssignees = V.singleton $ makeName @User login
    , newIssueMilestone = Nothing
    , newIssueLabels    = Nothing
    }
