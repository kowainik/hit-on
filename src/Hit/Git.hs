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

       , getUsername
       ) where

import Control.Exception (bracket)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Shellmet (($|))
import System.Directory (doesDirectoryExist, findExecutable, makeAbsolute, setCurrentDirectory)
import System.Process (callCommand)

import Hit.ColorTerminal (Answer (..), arrow, boldCode, errorMessage, greenCode, infoMessage,
                          prompt, resetCode)
import Hit.Core (CommitOptions (..), PushBool (..))
import Hit.Git.Status (showPrettyDiff)
import Hit.Issue (getIssueTitle, mkIssueId)

import qualified Data.Text as T


-- | @hit hop@ command.
runHop :: Maybe Text -> IO ()
runHop (nameOrMaster -> branch) = do
    "git" ["checkout",  branch]
    "git" ["pull", "--rebase", "--prune"]

-- | @hit fresh@ command.
runFresh :: Maybe Text -> IO ()
runFresh (nameOrMaster -> branch) = do
    "git" ["fetch", "origin", branch]
    "git" ["rebase", "origin/" <> branch]

-- | @hit new@ command.
runNew :: Text -> IO ()
runNew issueOrName = do
    login <- getUsername
    title <- case readMaybe @Int $ toString issueOrName of
        Just issueNum -> do
            issueTitle <- getIssueTitle $ mkIssueId issueNum
            pure $ show issueNum <> "-" <> mkShortDesc issueTitle
        Nothing -> pure $ mkShortDesc issueOrName
    let branchName = login <> "/" <> title
    "git" ["checkout", "-b", branchName]
  where
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

-- | @hit uncommit@ command
runUncommit :: IO ()
runUncommit = "git" ["reset", "HEAD~1"]

-- | @hit fix@ command
runFix :: Maybe Text -> PushBool -> IO ()
runFix msg pushBool = do
    "git" ["add", "."]
    "git" ["commit", "-m", message]
    runPush pushBool
  where
    message :: Text
    message = fromMaybe "Fix" msg

-- | @hit stash@ command: save all local changes to stash.
runStash :: IO ()
runStash = do
    "git" ["add", "."]
    "git" ["stash"]

-- | @hit unstash@ command: pop all saved changes.
runUnstash :: IO ()
runUnstash = "git" ["stash", "pop"]

-- | @hit amend@ command.
runAmend :: IO ()
runAmend = do
    "git" ["add", "."]
    "git" ["commit", "--amend", "--no-edit"]
    runPush Force

-- | @hit push@ command.
runPush :: PushBool -> IO ()
runPush isForce = getCurrentBranch >>= \branch ->
    "git" $ ["push", "--set-upstream", "origin", branch]
         ++ ["--force" | isForce == Force]

-- | @hit sync@ command.
runSync :: IO ()
runSync = getCurrentBranch >>= \branch ->
    "git" ["pull", "--rebase", "origin", branch]

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
            , "Are you sure you want to delete changes? " <> boldCode <> "[y]" <> resetCode <> "/n"
            ]
        prompt >>= \case
            N -> infoMessage "Aborting local clean up"
            Y -> clearChanges
  where
    clearChanges :: IO ()
    clearChanges = do
        "git" ["add", "."]
        "git" ["reset", "--hard"]

{- | Part of the @hit current@ command. Prints the current branch and returns
the current issue number if possible.
-}
runCurrent :: IO (Maybe Int)
runCurrent = do
    branchName <- getCurrentBranch
    putTextLn $ arrow <> "Current branch: " <> greenCode <> branchName <> resetCode
    pure $ issueFromBranch branchName

{- | Show stats from the given commit. If commit is not specified, uses HEAD.
-}
runStatus :: Maybe Text -> IO ()
runStatus (fromMaybe "HEAD" -> commit) = withUntrackedFiles $ showPrettyDiff commit

{- | Show diff from the given commit. If commit is not specified, uses HEAD.
This commands checks whether @diff-hightligh@ is on path and if not, just calls
@git diff@.
-}
runDiff :: Maybe Text -> IO ()
runDiff (fromMaybe "HEAD" -> commit) = withUntrackedFiles $
    findExecutable "diff-highlight" >>= \case
        Nothing -> "git" ["diff", commit]
        Just _  -> callCommand $ toString $
            "git diff " <> commit <> " --color=always | diff-highlight | less -rFX"

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
    (owner, repo) <- case T.splitOn "/" txt of
        [reponame] -> getUsername >>= \u -> pure (u, reponame)
        [username, reponame] -> pure (username, reponame)
        _ -> do
            errorMessage ("Incorrect name: " <> txt <> ". Use 'repo' or 'user/repo' formats")
            exitFailure
    let gitLink = "git@github.com:" <> owner <> "/" <> repo <> ".git"
    "git" ["clone", gitLink]
    putTextLn $ "⚙  cd " <> repo
    let repoDir = toString repo
    doesDirectoryExist repoDir >>= \case
        True -> makeAbsolute repoDir >>= setCurrentDirectory
        False -> errorMessage "Directory was not created, try again."

----------------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------------

-- | Get current user name from the local global git config.
getUsername :: IO Text
getUsername = do
    login <- "git" $| ["config", "user.login"]
    if login == ""
        then errorMessage "user.login is not specified" >> exitFailure
        else pure login

nameOrMaster :: Maybe Text -> Text
nameOrMaster = fromMaybe "master"

-- | Get the name of the current branch.
getCurrentBranch :: IO Text
getCurrentBranch = "git" $| ["rev-parse", "--abbrev-ref", "HEAD"]

{- | Extracts issue number from the branch in form like:

@
kowainik/<n>-short-description
@
-}
issueFromBranch :: Text -> Maybe Int
issueFromBranch =
      readMaybe
    . toString
    . T.takeWhile isDigit
    . T.drop 1
    . T.dropWhile (/= '/')

{- | Perform given action by adding all untracked files to index and returning
them back after action.
-}
withUntrackedFiles :: IO a -> IO a
withUntrackedFiles action = bracket
    addUntrackedFiles
    removeUntrackedFiles
    (const  action)
  where
    -- Add all untracked file to index so they will appear in diff
    addUntrackedFiles :: IO [Text]
    addUntrackedFiles = do
        untrackedFiles <- lines <$> "git" $| ["ls-files", "--others", "--exclude-standard"]
        for_ untrackedFiles $ \file -> void $ "git" $| ["add", file]
        pure untrackedFiles

    -- Return untracked files back to not spoil git state and have unexpected behavior
    removeUntrackedFiles :: [Text] -> IO ()
    removeUntrackedFiles = mapM_ $ \file -> void $ "git" $| ["reset", file]
