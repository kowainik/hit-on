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

import Hit.Core (IssueOptions (..), Milestone (..), Owner (..), Repo (..))
import Hit.Error (renderHitError)
import Hit.Git.Common (getUsername)
import Hit.GitHub.Auth (withAuthOwnerRepo)
import Hit.GitHub.Issue (Issue (..), ShortIssue (..), issueToShort, queryIssue, queryIssueList)
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

----------------------------------------------------------------------------
-- Multiple list processing
----------------------------------------------------------------------------

{- | Outputs the list of the open issues for the current project
with applied filters.

See 'getAllIssues' to find out more about filtering.
-}
printFilteredIssues
    :: Maybe Milestone  -- ^ Project Milestone
    -> Maybe Text  -- ^ User name of the assignee
    -> IO ()
printFilteredIssues milestone me = getAllIssues milestone me >>= printIssues

{- | Get the list of the opened issues for the current project
filtered out by the given input:

  * Only current user's issues?
  * Only issues from the current/specified milestone?
-}
getAllIssues
    :: Maybe Milestone  -- ^ Project Milestone
    -> Maybe Text  -- ^ User name of the assignee
    -> IO [ShortIssue]
getAllIssues milestone me = withAuthOwnerRepo queryIssueList >>= \case
    Left err -> errorMessage (renderHitError err) >> exitFailure
    Right is -> do
        milestoneId <- getMilestoneId milestone
        pure $ filterIssues milestoneId is
  where
    filterIssues :: Maybe (Id G.Milestone) -> [Issue] -> [Issue]
    filterIssues milestoneId =
        filter (\i -> my i && i `isInMilestone` milestoneId)

    my :: Issue -> Bool
    my issue = case me of
        Just (makeName -> username) -> username `isAssignedToIssue` issue
        Nothing                     -> True

    isInMilestone :: Issue -> Maybe (Id G.Milestone) -> Bool
    isInMilestone Issue{..} = \case
        Just milestoneId -> (milestoneNumber <$> issueMilestone) == Just milestoneId
        Nothing          -> True


-- | Create an 'Issue' by given title 'Text'
-- TODO: separate query to create issue in Hit.GitHub.Issue
createIssue :: Text -> Text -> Maybe (Id G.Milestone) -> IO (Either Error Issue)
createIssue title login milestone = withAuthOwnerRepo $ \token owner repo ->
    GitHub.createIssue token owner repo $ mkNewIssue title login milestone

{- | Assign the user to the given 'Issue'.

This function can fail assignment due to the following reasons:

 * Auth token fetch failure
 * Assignment query to GutHub failure

The function should inform user about corresponding 'Error' in each case and
continue working.
-}
-- TODO: separate query to assign to issue in Hit.GitHub.Issue
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

-- | Fetch only issue title.
-- TODO: separate GraphQL query to fetch only title
getIssueTitle :: Int -> IO Text
getIssueTitle num = issueTitle <$> fetchIssue num


{- | From the given 'Milestone' type try to get the milestone ID
-}
-- TODO: query to fetch the latest milestone in Hit.GitHub.Milestone
getMilestoneId :: Maybe Milestone -> IO (Maybe (Id G.Milestone))
getMilestoneId = \case
    Just (MilestoneId mId) -> pure $ Just $ mkId (Proxy @G.Milestone) mId
    Just CurrentMilestone  -> fetchCurrentMilestoneId
    Nothing                -> pure Nothing

{- | Fetches all open milestones. Then figure out the current one and return its
ID as 'Int'.

If it could not fetch, or there is no open milestones then prints a warning
message and returns 'Nothing'.
-}
fetchCurrentMilestoneId :: IO (Maybe (Id G.Milestone))
fetchCurrentMilestoneId = withOwnerRepo milestones' >>= \case
    Left err -> Nothing <$ warningMessage ("Could not fetch the milestones\n    " <> show err)
    Right ms -> case sortWith Down $ map milestoneNumber $ toList ms of
        []  -> warningMessage "There are no open milestones for this project" >> pure Nothing
        m:_ -> pure $ Just m

-- | Create new issue with title and assignee.
mkNewIssue :: Text -> Text -> Maybe (Id G.Milestone) -> NewIssue
mkNewIssue title login milestone = NewIssue
    { newIssueTitle     = title
    , newIssueBody      = Nothing
    , newIssueAssignees = V.singleton $ makeName @User login
    , newIssueMilestone = milestone
    , newIssueLabels    = Nothing
    }
