{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line interface for @hit@ executable.

module Hit.Cli
       ( hit
       ) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import Options.Applicative (Parser, ParserInfo, argument, auto, command, execParser, fullDesc, help,
                            helper, info, infoOption, long, metavar, progDesc, short, strArgument,
                            subparser, switch)

import Hit.ColorTerminal (arrow, blueCode, boldCode, redCode, resetCode)
import Hit.Git (runCommit, runCurrent, runFresh, runHop, runNew, runPush, runResolve, runSync)
import Hit.Issue (runIssue)

import qualified Data.Text as T
import qualified Paths_hit_on as Meta (version)


hit :: IO ()
hit = execParser cliParser >>= \case
    Hop branchName -> runHop branchName
    Fresh branchName -> runFresh branchName
    New issueNum -> runNew issueNum
    Issue issueNum -> runIssue issueNum
    Commit message noIssue -> runCommit message noIssue
    Resolve branchName -> runResolve branchName
    Push isForce -> runPush isForce
    Sync -> runSync
    Current -> runCurrent >>= flip whenJust (runIssue . Just)

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
cliParser :: ParserInfo HitCommand
cliParser = info ( helper <*> versionP <*> hitP )
    $ fullDesc <> progDesc "Haskell Git Helper Tool"

-- | Commands for
data HitCommand
    = Hop (Maybe Text)
    | Fresh (Maybe Text)
    | New Int
    | Issue (Maybe Int)
    | Commit Text Bool
    | Resolve (Maybe Text)
    | Push Bool
    | Sync
    | Current

-- | Commands parser.
hitP :: Parser HitCommand
hitP = subparser
    $ command "hop"     (info (helper <*> hopP)     $ progDesc "Switch to branch and sync it")
   <> command "fresh"   (info (helper <*> freshP)   $ progDesc "Rebase current branch on remote one")
   <> command "new"     (info (helper <*> newP)     $ progDesc "Create new branch from current one")
   <> command "commit"  (info (helper <*> commitP)  $ progDesc "Commit all local changes and prepend issue number")
   <> command "issue"   (info (helper <*> issueP)   $ progDesc "Show the information about the issue")
   <> command "push"    (info (helper <*> pushP)    $ progDesc "Push the current branch")
   <> command "sync"    (info (helper <*> syncP)    $ progDesc "Sync local branch with its remote")
   <> command "resolve" (info (helper <*> resolveP) $ progDesc "Switch to master, sync and delete the branch")
   <> command "current" (info (helper <*> currentP) $ progDesc "Show info about current branch and issue (if applicable)")

hopP :: Parser HitCommand
hopP = Hop <$> maybeBranchP

freshP :: Parser HitCommand
freshP = Fresh <$> maybeBranchP

newP :: Parser HitCommand
newP = New <$> issueNumP

issueP :: Parser HitCommand
issueP = Issue <$> optional issueNumP

commitP :: Parser HitCommand
commitP = do
    msg <- strArgument (metavar "COMMIT_MESSAGE")
    noIssue <- switch
        $ long "no-issue"
       <> short 'n'
       <> help "Do not add [#ISSUE_NUMBER] prefix when specified"
    pure $ Commit msg noIssue

pushP :: Parser HitCommand
pushP = Push <$> switch
    ( long "force"
   <> short 'f'
   <> help "Force push"
    )

syncP :: Parser HitCommand
syncP = pure Sync

currentP :: Parser HitCommand
currentP = pure Current

resolveP :: Parser HitCommand
resolveP = Resolve <$> maybeBranchP

-- | Parse optional branch name as an argument.
maybeBranchP :: Parser (Maybe Text)
maybeBranchP = optional $ strArgument (metavar "BRANCH_NAME")

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
