{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line interface for @hit@ executable.

module Hit.Cli
       ( hit
       ) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import Options.Applicative (Parser, ParserInfo, argument, auto, command, execParser, flag, fullDesc,
                            help, helper, info, infoOption, long, metavar, progDesc, short,
                            strArgument, subparser, switch)

import Hit.ColorTerminal (arrow, blueCode, boldCode, redCode, resetCode)
import Hit.Core (PushBool (..))
import Hit.Git (getUsername, runAmend, runClone, runCommit, runCurrent, runFix, runFresh, runHop,
                runNew, runPush, runResolve, runStatus, runSync)
import Hit.Issue (runIssue)

import qualified Data.Text as T
import qualified Paths_hit_on as Meta (version)


hit :: IO ()
hit = execParser cliParser >>= \case
    Hop branchName -> runHop branchName
    Fresh branchName -> runFresh branchName
    New issueNum -> runNew issueNum
    Issue issueNum me -> if me
        then getUsername >>= runIssue issueNum . Just
        else runIssue issueNum Nothing
    Commit message noIssue -> runCommit message noIssue
    Fix message pushBool -> runFix message pushBool
    Amend -> runAmend
    Resolve branchName -> runResolve branchName
    Push isForce -> runPush isForce
    Sync -> runSync
    Current -> runCurrent >>= flip whenJust (flip runIssue Nothing . Just)
    Status commit -> runCurrent >> runStatus commit
    Clone name -> runClone name

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
cliParser :: ParserInfo HitCommand
cliParser = info ( helper <*> versionP <*> hitP )
    $ fullDesc <> progDesc "Haskell Git Helper Tool"

-- | Commands for @hit@ executable.
data HitCommand
    = Hop (Maybe Text)
    | Fresh (Maybe Text)
    | New Int
    | Issue (Maybe Int) Bool
    | Commit
        {- | Commit name. If not specified use the issue name.
        If issue number is not applicable do not perform any actions.
        -}
        (Maybe Text)
        -- | Do not use the issue num in the commit name
        Bool
    | Fix
        (Maybe Text)  -- ^ Text of the fix commit
        PushBool      -- ^ Force push
    | Amend
    | Resolve (Maybe Text)
    | Push PushBool
    | Sync
    | Current
    | Status (Maybe Text)
    | Clone Text

-- | Commands parser.
hitP :: Parser HitCommand
hitP = subparser
    $ command "hop"     (info (helper <*> hopP)     $ progDesc "Switch to branch and sync it")
   <> command "fresh"   (info (helper <*> freshP)   $ progDesc "Rebase current branch on remote one")
   <> command "new"     (info (helper <*> newP)     $ progDesc "Create new branch from current one")
   <> command "commit"  (info (helper <*> commitP)  $ progDesc "Commit all local changes and prepend issue number")
   <> command "fix"     (info (helper <*> fixP)     $ progDesc "Fix requested changes to the last commit")
   <> command "amend"   (info (helper <*> amendP)   $ progDesc "Amend changes to the last commit and force push")
   <> command "issue"   (info (helper <*> issueP)   $ progDesc "Show the information about the issue")
   <> command "push"    (info (helper <*> pushP)    $ progDesc "Push the current branch")
   <> command "sync"    (info (helper <*> syncP)    $ progDesc "Sync local branch with its remote")
   <> command "resolve" (info (helper <*> resolveP) $ progDesc "Switch to master, sync and delete the branch")
   <> command "current" (info (helper <*> currentP) $ progDesc "Show info about current branch and issue (if applicable)")
   <> command "status"  (info (helper <*> statusP)  $ progDesc "Show current branch and beautiful diff with COMMIT_HASH (by default HEAD)")
   <> command "clone"   (info (helper <*> cloneP)   $ progDesc "Clone the repo. Use 'reponame' or 'username/reponame' formats")

hopP :: Parser HitCommand
hopP = Hop <$> maybeBranchP

freshP :: Parser HitCommand
freshP = Fresh <$> maybeBranchP

newP :: Parser HitCommand
newP = New <$> issueNumP

issueP :: Parser HitCommand
issueP = do
    num <- optional issueNumP
    me <- switch
        $ long "me"
       <> short 'm'
       <> help "Assigned to me"
    pure $ Issue num me

commitP :: Parser HitCommand
commitP = do
    msg <- optional $ strArgument (metavar "COMMIT_MESSAGE")
    noIssue <- switch
        $ long "no-issue"
       <> short 'n'
       <> help "Do not add [#ISSUE_NUMBER] prefix when specified"
    pure $ Commit msg noIssue

{- HLINT ignore "Use <$>"-}
fixP :: Parser HitCommand
fixP = do
    commitMsg <- commitMessageP
    isForce   <- pushBoolP
    pure $ Fix commitMsg isForce

amendP :: Parser HitCommand
amendP = pure Amend

pushP :: Parser HitCommand
pushP = Push <$> pushBoolP

syncP :: Parser HitCommand
syncP = pure Sync

currentP :: Parser HitCommand
currentP = pure Current

statusP :: Parser HitCommand
statusP = Status <$> optional (strArgument $ metavar "COMMIT_HASH")

resolveP :: Parser HitCommand
resolveP = Resolve <$> maybeBranchP

cloneP :: Parser HitCommand
cloneP = Clone <$> strArgument (metavar "REPOSITORY")

-- | Parse optional branch name as an argument.
maybeBranchP :: Parser (Maybe Text)
maybeBranchP = optional $ strArgument (metavar "BRANCH_NAME")

-- | Parse optional commit message as an argument
commitMessageP :: Parser (Maybe Text)
commitMessageP = optional $ strArgument (metavar "COMMIT_MESSAGE")

-- | Parse flag of force push.
pushBoolP :: Parser PushBool
pushBoolP = flag Simple Force
    ( long "force"
   <> short 'f'
   <> help "Force push"
    )

-- | Parse issue number as an argument.
issueNumP :: Parser Int
issueNumP = argument auto $ metavar "ISSUE_NUMBER"

-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption hitVersion
    $ long "version"
   <> short 'v'
   <> help "Show hit's version"

hitVersion :: String
hitVersion = toString
    $ T.intercalate "\n"
    $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    blueBold txt = blueCode <> boldCode <> txt <> resetCode
    sVersion = blueBold "Hit " <> "v" <> toText (showVersion Meta.version)
    sHash = arrow <> blueBold "Git revision: " <> $(gitHash)
    sDate = arrow <> blueBold "Commit date:  " <> $(gitCommitDate)
    sDirty = redCode <> "There are non-committed files." <> resetCode
