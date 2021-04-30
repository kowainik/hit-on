{- |
Module                  : Hit.Git.Branch
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

All functionality related to the branch creation and manipulation.

@hit new@ and @hit rename@ command runners and helpers.
-}

module Hit.Git.Branch
    ( runNew
    , runRename

      -- * Branch naming helpers
    , BranchDescription (..)
    , assignAndDisplayBranchDescription
    , mkBranchDescription
    ) where

import Colourista (errorMessage, infoMessage, successMessage, warningMessage)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Relude.Extra.Bifunctor (secondF)
import Shellmet (($?))

import Hit.Core (IssueNumber (..), MilestoneOption, NewOptions (..), newOptionsWithName)
import Hit.Error (HitError, renderHitError)
import Hit.Formatting (stripRfc)
import Hit.Git.Common (getCurrentBranch, getMainBranch, getUsername, meToUsername)
import Hit.Git.Issue (fetchIssue, getAllIssues, getMilestoneNumber, printIssues, showIssueNumber)
import Hit.GitHub (CreatedIssue (..), Issue (..), ShortIssue (..), assignUserToIssue,
                   mutationCreateNewIssue, queryMilestoneId, queryMyId, withAuthOwnerRepo)

import qualified Data.Text as Text
import qualified GitHub as GH


-- | @hit new@ command.
runNew :: NewOptions -> IO ()
runNew NewOptions{..} = do
    issueOrBranch <- case noIssueOrBranch of
        Just issueOrBranch -> pure issueOrBranch
        Nothing -> if noMe
            then meToUsername noMe >>= getAllIssues Nothing >>= \case
                 [] -> do
                     warningMessage "There is no issues assign to you at the moment"
                     exitFailure
                 [issue] -> pure $ show $ unIssueNumber $ shortIssueNumber issue
                 issues -> do
                     infoMessage "Here is the list of the issues assigned to you:"
                     printIssues issues
                     exitFailure
            else do
                errorMessage "You should specify either the issue number, or branch name"
                errorMessage "Or you can use '--me' option to create a branch of issue assign to you."
                exitFailure

    branchName <- mkBranchName noCreateIssue noMilestone issueOrBranch
    "git" ["checkout", "-b", branchName]

{- | @hit rename@ command.

Renames the current (non-main) branch to the given name or issue with the
username prefix.

If the current branch is the main one, it just creates a new branch as in 'runNew'.
-}
runRename :: Text -> IO ()
runRename issueOrName = do
    curBranch <- getCurrentBranch
    mainBranch <- getMainBranch
    if curBranch == mainBranch
    then runNew $ newOptionsWithName issueOrName
    else do
        newBranch <- mkBranchName False Nothing issueOrName
        -- rename a current branch locally
        "git" ["branch", "-m", curBranch, newBranch]
        -- check if the old branch is on remote
        isRemote <- (True <$ "git" ["ls-remote", "--exit-code", "--heads", "origin", curBranch]) $? pure False
        -- rename the corresponding branch remote (if applicable)
        when isRemote $ do
            "git" ["push", "origin", "-u", newBranch]
            "git" ["push", "origin", "--delete", curBranch]

{- | This function returns the new branch name that should be used.

If specified, the new corresponding issue is created and assigned.
If the provided new name is issue number then it should assign the user to the issue and create the branch in the following view:

@
username/42-Issue-Title-From-GitHub
@

The issue title is taken from the corresponding issue and escaped.
-}
mkBranchName
    :: Bool  -- ^ if the new issue should be created
    -> Maybe MilestoneOption  -- ^ Add the new issue to the milestone?
    -> Text  -- ^ user input: issue number or text
    -> IO Text
mkBranchName doCreateIssue milestoneOpt issueOrName = do
    maybeIssue <- if doCreateIssue then tryCreateNewIssue else pure Nothing

    let branchDescription = mkBranchDescription
            (createdIssueNumber <$> maybeIssue)
            issueOrName

    login <- getUsername
    title <- assignAndDisplayBranchDescription True login branchDescription
    pure $ login <> "/" <> title
  where
    tryCreateNewIssue :: IO (Maybe CreatedIssue)
    tryCreateNewIssue = do
        infoMessage $ "Creating issue with title: '" <> issueOrName <> "'"
        createIssue issueOrName milestoneOpt >>= \case
            Left err -> do
                errorMessage "Error creating issue under 'hit new' command!"
                putTextLn $ renderHitError err
                pure Nothing
            Right issue -> do
                successMessage $ "Successfully created issue number #"
                    <> show (unIssueNumber $ createdIssueNumber issue)
                showIssueLink $ createdIssueUrl issue
                pure $ Just issue


{- | This data type represents all cases on how to create short branch
name description. During 'hit new' command there can be several cases:

1. 'FromNewIssue': when new issue is created, we know its title and number.
2. 'FromIssueNumber': if issue is not created, we need to fetch its title by id.
3. 'FromText': if not issue number is provided, we just create raw text.
-}
data BranchDescription
    = FromNewIssue IssueNumber Text
    | FromIssueNumber IssueNumber
    | FromText Text


-- | Create 'BranchTitle' from possible issue and issue number or text.
mkBranchDescription :: Maybe IssueNumber -> Text -> BranchDescription
mkBranchDescription (Just issueNum) title = FromNewIssue issueNum title
mkBranchDescription Nothing issueOrName = case readMaybe @Int $ toString issueOrName of
    Just issueNum -> FromIssueNumber $ IssueNumber issueNum
    Nothing       -> FromText issueOrName

{- | Assigns the user to the issue if applicable (in the current design, if the
issue already exists and user creates the branch for it: 'FromIssueNumber').

Displays 'BranchDescription' in format:

@
123-short-issue-title
@
-}
assignAndDisplayBranchDescription
    :: Bool  -- ^ To assign the given user to the issue?
    -> Text  -- ^ User name
    -> BranchDescription
    -> IO Text
assignAndDisplayBranchDescription doAssign username = \case
    FromText text -> pure $ mkShortDesc text
    FromNewIssue issueNum issueTitle -> pure $ nameWithNumber issueNum issueTitle
    FromIssueNumber issueNum -> do
        -- TODO: fetch less: only ID, assignees, title and number
        issue <- fetchIssue issueNum
        when doAssign $ do
            assignToIssue issue username
            showIssueLink $ issueUrl issue
        pure $ nameWithNumber issueNum (issueTitle issue)
  where
    nameWithNumber :: IssueNumber -> Text -> Text
    nameWithNumber (IssueNumber issueNum) issueTitle =
        show issueNum <> "-" <> mkShortDesc issueTitle

    mkShortDesc :: Text -> Text
    mkShortDesc =
          Text.intercalate "-"
        . take 5
        . words
        . Text.filter
            (\c -> isAlphaNum c
                || isDigit c
                || isSpace c
                || c `elem` ("_-/" :: String)
            )
        . stripRfc

showIssueLink :: Text -> IO ()
showIssueLink url = infoMessage $ "  Issue link: " <> url

-- | Create an 'Issue' by given title 'Text'
createIssue :: Text -> Maybe MilestoneOption -> IO (Either HitError CreatedIssue)
createIssue title milestoneOpt = withAuthOwnerRepo $ \token owner repo -> do
    -- TODO: optimize to 2 calls instead of 3
    -- Also, it's so awkward to work with 'IO (Either ...)', but there's no better way...
    milestoneNumber <- getMilestoneNumber milestoneOpt

    eMilestoneId <- case milestoneNumber of
        Nothing  -> pure $ Right Nothing
        Just mId -> secondF Just $ queryMilestoneId token owner repo mId

    case eMilestoneId of
        Left err          -> pure $ Left err
        Right milestoneId -> mutationCreateNewIssue token owner repo title milestoneId

{- | Assign the user to the given 'Issue'.

This function can fail assignment due to the following reasons:

 * Auth token fetch failure
 * Assignment query to GutHub failure

The function should inform user about corresponding 'Error' in each case and
continue working.
-}
assignToIssue :: Issue -> Text -> IO ()
assignToIssue Issue{..} username = do
    res <- withAuthOwnerRepo $ \token _owner _repo ->
        if username `elem` issueAssignees
            then pure $ Right (issueNumber, True)
            else secondF (, False) (addAssignee token issueId)

    case res of
        Right (iss, isAlreadyAssigned) ->
            if isAlreadyAssigned
            then pass
            else successMessage $ "You were assigned to the issue #" <> showIssueNumber iss
        Left err  -> do
            errorMessage "Can not assign you to the issue."
            putTextLn $ "    " <> renderHitError err

addAssignee :: GH.GitHubToken -> GH.IssueId -> IO (Either GH.GitHubError IssueNumber)
addAssignee token issueId = queryMyId token >>= \case
    Left err   -> pure $ Left err
    Right myId -> assignUserToIssue token myId issueId
