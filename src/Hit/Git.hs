{-# LANGUAGE ViewPatterns #-}

-- | Logic for CLI commands to make GitHub workflows easier.

module Hit.Git
       ( runHop
       , runFresh
       , runNew
       , runPush
       , runResolve
       , runStash
       , runUnstash
       , runCommit
       , runUncommit
       , runFix
       , runAmend
       , runSync
       , runClear
       , runCurrent
       , runStatus
       , runDiff
       , runClone
       , runLog

       , getUsername
       ) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import GitHub (Issue (issueNumber), IssueNumber (..), unIssueNumber)

import Hit.ColorTerminal (Answer (..), errorMessage, infoMessage, prompt
                         , successMessage, yesOrNoText)
import Hit.Core (CommitOptions (..), PushBool (..))
import Hit.Issue (createIssue, getIssueTitle, mkIssueId)

import qualified Data.Text as T

import Hit.Git.Current (runCurrent)
import Hit.Git.Diff (runDiff)
import Hit.Git.Fix (runFix)
import Hit.Git.Fresh (runFresh)
import Hit.Git.Hop (runHop)
import Hit.Git.Log (runLog)
import Hit.Git.Push (runPush)
import Hit.Git.Stash (runStash)
import Hit.Git.Status (runStatus)
import Hit.Git.Sync (runSync)
import Hit.Git.Uncommit (runUncommit)
import Hit.Git.Unstash (runUnstash)

import Hit.Git.Common 
    ( nameOrMaster
    , getUsername
    , getCurrentBranch
    , issueFromBranch
    )

-- QUESTION: should we somehow move this into separate module or split this module
--           smaller parts?
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

{- | Display 'BranchDescription' in format:

@
123-short-issue-title
@
-}
displayBranchDescription :: BranchDescription -> IO Text
displayBranchDescription = \case
    FromText text -> pure $ mkShortDesc text
    FromNewIssue issueNum issueTitle -> pure $ nameWithNumber issueNum issueTitle
    FromIssueNumber issueNum -> do
        issueTitle <- getIssueTitle $ mkIssueId issueNum
        pure $ nameWithNumber issueNum issueTitle
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

-- | @hit new@ command.
runNew :: Bool -> Text -> IO ()
runNew isIssue issueOrName = do
    login <- getUsername
    maybeIssue <- if isIssue then tryCreateNewIssue login else pure Nothing
    let branchDescription = mkBranchDescription maybeIssue issueOrName
    title <- displayBranchDescription branchDescription
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
                pure $ Just issueNum

-- | @hit commit@ command.
runCommit :: CommitOptions -> IO ()
runCommit CommitOptions{..} = case coName of
    Just (T.strip -> msg)
        | msg == "" -> errorMessage "Commit message cannot be empty"
        | otherwise -> getCurrentIssue >>= commitCmds msg
    {- if the commit name is not specified then check the branchName
    If this is issue-related branch, take the issue name as the commit name.
    Otherwise print errorMessage.
    -}
    Nothing -> do
        issueNum <- getCurrentIssue
        case issueNum of
            Nothing -> errorMessage "Commit message cannot be empty: can not be taken from the context"
            Just n -> do
                title <- getIssueTitle (mkIssueId n)
                commitCmds title issueNum
  where
    commitCmds :: Text -> Maybe Int -> IO ()
    commitCmds msg issueNum = do
        "git" ["add", "."]
        "git" ["commit", "-m", showMsg msg $ guard hasIssue *> issueNum]
        when (coPush || coIsForcePush == Force) $ runPush coIsForcePush

    getCurrentIssue :: IO (Maybe Int)
    getCurrentIssue = issueFromBranch <$> getCurrentBranch

    showMsg :: Text -> Maybe Int -> Text
    showMsg msg = \case
       Nothing -> msg
       Just n  ->
           let issue = "#" <> show n
           in "[" <> issue <> "] " <> msg <> "\n\nResolves " <> issue

    hasIssue :: Bool
    hasIssue = not coNoIssueNumber

-- | @hit amend@ command.
runAmend :: Bool -> IO ()
runAmend localAmend = do
    "git" ["add", "."]
    "git" ["commit", "--amend", "--no-edit"]
    unless localAmend $ runPush Force

-- | @hit resolve@ command.
runResolve :: Maybe Text -> IO ()
runResolve (nameOrMaster -> master)= do
    curBranch <- getCurrentBranch
    runHop $ Just master
    when (curBranch /= master) $ "git" ["branch", "-D", curBranch]

-- | Remove all local changes permanently.
runClear :: PushBool -> IO ()
runClear = \case
    Force  -> clearChanges
    Simple -> do
        putText $ unlines
            [ "This command permanently deletes all uncommited changes"
            , "Hint: if you want to save changes, use 'hit stash' command."
            , "Are you sure you want to delete changes? " <> yesOrNoText N
            ]
        prompt N >>= \case
            N -> infoMessage "Aborting local clean up"
            Y -> clearChanges
  where
    clearChanges :: IO ()
    clearChanges = do
        "git" ["add", "."]
        "git" ["reset", "--hard"]

{- | @hit clone@ command receives the name of the repo in the following
formats:

* @reponame@ — current user's username is used to clone the repo from.
* @name/reponame@ — specified GitHub username is used to clone the repo from.

__Note__ that the @ssh@ strategy is used for cloning from GitHub. See the corresponding @git@ command:

@
git clone git@github.com:username/project-name.git
@
-}
runClone :: Text -> IO ()
runClone txt = do
    name <- case T.splitOn "/" txt of
        [reponame] -> getUsername >>= \u -> pure $ u <> "/" <> reponame
        [username, reponame] -> pure $ username <> "/" <> reponame
        _ -> do
            errorMessage ("Incorrect name: " <> txt <> ". Use 'repo' or 'user/repo' formats")
            exitFailure
    let gitLink = "git@github.com:" <> name <> ".git"
    "git" ["clone", gitLink]
