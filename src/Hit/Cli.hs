{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line interface for @hit@ executable.

module Hit.Cli
       ( hit
       ) where

import Colourista (blue, bold, formatWith)
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Options.Applicative (CommandFields, Mod, Parser, ParserInfo, argument, auto, command,
                            execParser, flag, flag', fullDesc, help, helper, info, infoOption, long,
                            metavar, option, progDesc, short, strArgument, subparser, switch)

import Hit.Core (CommitOptions (..), IssueOptions (..), Milestone (..), PushBool (..),
                 defaultIssueOptions)
import Hit.Git (runAmend, runClear, runClone, runCommit, runCurrent, runDiff, runFix, runFresh,
                runHop, runLog, runNew, runPush, runResolve, runStash, runStatus, runSync,
                runUncommit, runUnstash)
import Hit.Issue (runIssue)
import Hit.Prompt (arrow)

import qualified Data.Text as T
import qualified Paths_hit_on as Meta (version)


hit :: IO ()
hit = execParser cliParser >>= \case
    Hop branchName -> runHop branchName
    Fresh branchName -> runFresh branchName
    New createIssue issueNum -> runNew createIssue issueNum
    Issue issueOpts -> runIssue issueOpts
    Stash -> runStash
    Unstash -> runUnstash
    Commit opts -> runCommit opts
    Uncommit -> runUncommit
    Fix message pushBool -> runFix message pushBool
    Amend localAmend -> runAmend localAmend
    Resolve branchName -> runResolve branchName
    Push isForce -> runPush isForce
    Sync -> runSync
    Clear isForce -> runClear isForce
    Current -> runCurrent >>= flip whenJust (\i -> runIssue defaultIssueOptions {ioIssueNumber = Just i})
    Status commit -> runCurrent >> runStatus commit
    Diff commit -> runDiff commit
    Clone name -> runClone name
    Log commit -> runLog commit

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
    | New
        Bool  -- ^ Should create issue as well?
        Text  -- ^ Issue or branch name
    | Issue IssueOptions
    | Stash
    | Unstash
    | Commit CommitOptions
    | Uncommit
    | Fix
        (Maybe Text)  -- ^ Text of the fix commit
        PushBool      -- ^ Force push
    | Amend
        Bool  -- ^ Local amend
    | Resolve (Maybe Text)
    | Push PushBool
    | Sync
    | Clear PushBool
    | Current
    | Status (Maybe Text)
    | Diff (Maybe Text)
    | Clone Text
    | Log (Maybe Text)

-- | Commands parser.
hitP :: Parser HitCommand
hitP = subparser
    $ com "hop"      hopP      "Switch to branch and sync it"
   <> com "fresh"    freshP    "Rebase current branch on remote one"
   <> com "new"      newP      "Create new branch from the current one"
   <> com "stash"    stashP    "Stash all local changes"
   <> com "unstash"  unstashP  "Unstash previously stashed changes"
   <> com "commit"   commitP   "Commit all local changes and prepend issue number"
   <> com "uncommit" uncommitP "Reset to the previous commit saving the changes"
   <> com "fix"      fixP      "Fix requested changes to the last commit"
   <> com "amend"    amendP    "Amend changes to the last commit and force push"
   <> com "issue"    issueP    "Show the information about the issue"
   <> com "push"     pushP     "Push the current branch"
   <> com "sync"     syncP     "Sync local branch with its remote"
   <> com "resolve"  resolveP  "Switch to master, sync and delete the branch"
   <> com "clear"    clearP    "Remove all local changes permanently"
   <> com "current"  currentP  "Show info about current branch and issue (if applicable)"
   <> com "status"   statusP   "Show current branch and beautiful stats with COMMIT_HASH (by default HEAD)"
   <> com "diff"     diffP     "Display beautiful diff with COMMIT_HASH (by default HEAD)"
   <> com "clone"    cloneP    "Clone the repo. Use 'reponame' or 'username/reponame' formats"
   <> com "log"      logP      "Display the log of the current commit or COMMIT_HASH"
  where
    com :: String -> Parser HitCommand -> String -> Mod CommandFields HitCommand
    com name p desc = command name (info (helper <*> p) $ progDesc desc)

hopP :: Parser HitCommand
hopP = Hop <$> maybeBranchP

freshP :: Parser HitCommand
freshP = Fresh <$> maybeBranchP

newP :: Parser HitCommand
newP = do
    createIssue <- switch
        $ long "issue"
       <> short 'i'
       <> help "Create new issue in addition to branch and assign it to you"
    issueNumOrBranch <- strArgument (metavar "ISSUE_NUMBER_OR_BRANCH_NAME")
    pure $ New createIssue issueNumOrBranch

issueP :: Parser HitCommand
issueP = do
    ioIssueNumber <- optional issueNumP
    ioMe <- switch
        $ long "me"
       <> help "Assigned to me"
    ioMilestone <- milestoneP
    pure $ Issue IssueOptions {..}

stashP :: Parser HitCommand
stashP = pure Stash

unstashP :: Parser HitCommand
unstashP = pure Unstash

commitP :: Parser HitCommand
commitP = do
    coName <- optional $ strArgument (metavar "COMMIT_MESSAGE")
    coNoIssueNumber <- switch
        $ long "no-issue"
       <> short 'n'
       <> help "Do not add [#ISSUE_NUMBER] prefix when specified"
    coPush <- switch
        $ long "push"
       <> short 'p'
       <> help "Push current branch with this commit"
    coIsForcePush <- pushBoolP
    pure $ Commit CommitOptions{..}

uncommitP :: Parser HitCommand
uncommitP = pure Uncommit

{- HLINT ignore "Use <$>"-}
fixP :: Parser HitCommand
fixP = do
    commitMsg <- commitMessageP
    isForce   <- pushBoolP
    pure $ Fix commitMsg isForce

amendP :: Parser HitCommand
amendP = do
    localAmend <- switch
        $ long "local"
       <> short 'l'
       <> help "Whether to do a local amend only - without pushing"
    pure $ Amend localAmend

pushP :: Parser HitCommand
pushP = Push <$> pushBoolP

syncP :: Parser HitCommand
syncP = pure Sync

clearP :: Parser HitCommand
clearP = Clear <$> pushBoolP

currentP :: Parser HitCommand
currentP = pure Current

statusP :: Parser HitCommand
statusP = Status <$> maybeCommitP

diffP :: Parser HitCommand
diffP = Diff <$> maybeCommitP

resolveP :: Parser HitCommand
resolveP = Resolve <$> maybeBranchP

cloneP :: Parser HitCommand
cloneP = Clone <$> strArgument (metavar "REPOSITORY")

logP :: Parser HitCommand
logP = Log <$> maybeCommitP

-- | Parse optional branch name as an argument.
maybeBranchP :: Parser (Maybe Text)
maybeBranchP = optional $ strArgument $ metavar "BRANCH_NAME"

-- | Parse optional commit hash as an argument.
maybeCommitP :: Parser (Maybe Text)
maybeCommitP = optional $ strArgument $ metavar "COMMIT_HASH"

-- | Parse optional commit message as an argument
commitMessageP :: Parser (Maybe Text)
commitMessageP = optional $ strArgument $ metavar "COMMIT_MESSAGE"

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

milestoneP :: Parser (Maybe Milestone)
milestoneP = optional (curMilestone <|> milestoneId)
  where
    curMilestone :: Parser Milestone
    curMilestone = flag' CurrentMilestone $ mconcat
        [ long "current-milestone"
        , short 'm'
        , help "Use the Project's current Milestone"
        ]

    milestoneId :: Parser Milestone
    milestoneId = MilestoneId <$> option auto
        ( long "milestone"
        <> help "Specify the project's Milestone ID"
        <> metavar "MILESTONE_ID"
        )

-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption hitVersion
    $ long "version"
   <> short 'v'
   <> help "Show hit's version"

hitVersion :: String
hitVersion = toString $ T.intercalate "\n"
    [ sVersion
    , sHash
    , sDate
    ]
  where
    blueBold = formatWith [blue, bold]
    sVersion = blueBold "Hit " <> "v" <> toText (showVersion Meta.version)
    sHash = arrow <> blueBold "Git revision: " <> $(gitHash)
    sDate = arrow <> blueBold "Commit date:  " <> $(gitCommitDate)
