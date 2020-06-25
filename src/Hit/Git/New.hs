-- | Everything related to the `hit new` command

module Hit.Git.New
    ( runNew
    ) where

import Data.Char (isAlphaNum, isDigit, isSpace)

import Colourista (errorMessage, infoMessage, successMessage)
import GitHub (Issue (issueHtmlUrl, issueNumber, issueTitle), IssueNumber (..), getUrl,
               unIssueNumber)

import Hit.Formatting (stripRfc)
import Hit.Git.Common (getUsername)
import Hit.Issue (assignIssue, createIssue, fetchIssue, mkIssueId)

import qualified Data.Text as T


-- | @hit new@ command.
runNew :: Bool -> Text -> IO ()
runNew isIssue issueOrName = do
    login <- getUsername
    maybeIssue <- if isIssue then tryCreateNewIssue login else pure Nothing
    let branchDescription = mkBranchDescription maybeIssue issueOrName
    title <- assignAndDisplayBranchDescription login branchDescription
    let branchName = login <> "/" <> title
    "git" ["checkout", "-b", branchName]
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

{- | Assigns the user to the issue if applicable (it current design, if the issue
already exists and user creates the branch for it: 'FromIssueNumber').

Displays 'BranchDescription' in format:

@
123-short-issue-title
@
-}
assignAndDisplayBranchDescription :: Text -> BranchDescription -> IO Text
assignAndDisplayBranchDescription username = \case
    FromText text -> pure $ mkShortDesc text
    FromNewIssue issueNum issueTitle -> pure $ nameWithNumber issueNum issueTitle
    FromIssueNumber issueNum -> do
        issue <- fetchIssue $ mkIssueId issueNum
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
                       || c `elem` ("_-./" :: String)
                   )
        . stripRfc

showIssueLink :: Issue -> IO ()
showIssueLink issue = whenJust (issueHtmlUrl issue) $ \url ->
    infoMessage $ "  Issue link: " <> getUrl url
