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

import Data.Char (isAlphaNum, isDigit, isSpace)

import Colourista (errorMessage, infoMessage, successMessage, warningMessage)
import GitHub (Issue (issueHtmlUrl, issueNumber, issueTitle), IssueNumber (..), getUrl,
               unIssueNumber)
import Shellmet (($?))

import Hit.Core (NewOptions (..), newOptionsWithName)
import Hit.Formatting (stripRfc)
import Hit.Git.Common (getCurrentBranch, getUsername)
import Hit.Issue (assignIssue, createIssue, fetchIssue, getAllIssues, meToUsername, mkIssueId,
                  printIssues)

import qualified Data.Text as T
import qualified Data.Vector as V


-- | @hit new@ command.
runNew :: NewOptions -> IO ()
runNew NewOptions{..} = do
    issueOrBranch <- case noIssueOrBranch of
        Just issueOrBranch -> pure issueOrBranch
        Nothing -> if noMe
            then meToUsername noMe >>= getAllIssues Nothing >>= \is -> case length is of
                 0 -> warningMessage "There is no issues assign to you at the moment" >> exitFailure
                 1 -> pure $ show $ unIssueNumber $ issueNumber $ V.head is
                 _n  -> do
                     infoMessage "Here is the list of the issues assigned to you:" >> printIssues is
                     exitFailure

            else do
                errorMessage "You should specify either the issue number, or branch name"
                errorMessage "Or you can use '--me' option to create a branch of issue assign to you."
                exitFailure
    branchName <- mkBranchName noCreateIssue issueOrBranch
    "git" ["checkout", "-b", branchName]

{- | @hit rename@ command.

Renames the current (non-master) branch to the given name or issue with the
username prefix.

If the current branch is master, it just creates a new branch as in 'runNew'.
-}
runRename :: Text -> IO ()
runRename issueOrName = do
    curBranch <- getCurrentBranch
    if curBranch == "master"
    then runNew $ newOptionsWithName issueOrName
    else do
        newBranch <- mkBranchName False issueOrName
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
    -> Text  -- ^ user input: issue number or text
    -> IO Text
mkBranchName doCreateIssue issueOrName = do
    login <- getUsername
    maybeIssue <- if doCreateIssue then tryCreateNewIssue login else pure Nothing
    let branchDescription = mkBranchDescription maybeIssue issueOrName
    title <- assignAndDisplayBranchDescription True login branchDescription
    pure $ login <> "/" <> title
  where
    tryCreateNewIssue :: Text -> IO (Maybe IssueNumber)
    tryCreateNewIssue login = do
        infoMessage $ "Creating issue with title: '" <> issueOrName <> "'"
        createIssue issueOrName login >>= \case
            Left err -> do
                errorMessage "Error creating issue under 'hit new' command!"
                putTextLn $ show err
                pure Nothing
            Right issue -> do
                let issueNum = issueNumber issue
                successMessage $ "Successfully created issue number #"
                    <> show (unIssueNumber issueNum)
                showIssueLink issue
                pure $ Just issueNum


{- | This data type represents all cases on how to create short branch
name description. During 'hit new' command there can be several cases:

1. 'FromNewIssue': when new issue is created, we know its title and number.
2. 'FromIssueNumber': if issue is not created, we need to fetch its title by id.
3. 'FromText': if not issue number is provided, we just create raw text.
-}
data BranchDescription
    = FromNewIssue Int Text
    | FromIssueNumber Int
    | FromText Text


-- | Create 'BranchTitle' from possible issue and issue number or text.
mkBranchDescription :: Maybe IssueNumber -> Text -> BranchDescription
mkBranchDescription (Just issueNum) title = FromNewIssue (unIssueNumber issueNum) title
mkBranchDescription Nothing issueOrName = case readMaybe @Int $ toString issueOrName of
    Just issueNum -> FromIssueNumber issueNum
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
        issue <- fetchIssue $ mkIssueId issueNum
        when doAssign $ do
            assignIssue issue username
            showIssueLink issue
        pure $ nameWithNumber issueNum $ issueTitle issue
  where
    nameWithNumber :: Int -> Text -> Text
    nameWithNumber issueNum issueTitle =
        show issueNum <> "-" <> mkShortDesc issueTitle

    mkShortDesc :: Text -> Text
    mkShortDesc =
          T.intercalate "-"
        . take 5
        . words
        . T.filter (\c -> isAlphaNum c
                       || isDigit c
                       || isSpace c
                       || c `elem` ("_-/" :: String)
                   )
        . stripRfc

showIssueLink :: Issue -> IO ()
showIssueLink issue = whenJust (issueHtmlUrl issue) $ \url ->
    infoMessage $ "  Issue link: " <> getUrl url
