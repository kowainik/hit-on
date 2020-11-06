{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module                  : Hit.Cli
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Command line interface for @hit@ executable.
-}

module Hit.Cli
    ( hit
    ) where

import Colourista (blue, bold, formatWith)
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Options.Applicative (CommandFields, Mod, Parser, ParserInfo, argument, auto, command,
                            execParser, flag, flag', fullDesc, help, helper, info, infoOption, long,
                            metavar, option, progDesc, short, strArgument, strOption, subparser,
                            switch)

import Hit.Core (CommitOptions (..), ForceFlag (..), IssueOptions (..), Milestone (..),
                 NewOptions (..), defaultIssueOptions)
import Hit.Git (runAmend, runClear, runClone, runCommit, runCurrent, runDiff, runFix, runFork,
                runFresh, runHop, runLog, runMilestones, runNew, runPr, runPush, runRename,
                runResolve, runStatus, runSync, runUncommit, runWip)
import Hit.Git.Stash (runStash, runStashClear, runStashDiff, runStashList, runUnstash)
import Hit.Issue (runIssue)
import Hit.Prompt (arrow)

import qualified Data.Text as T
import qualified Paths_hit_on as Meta (version)


hit :: IO ()
hit = execParser cliParser >>= \case
    Hop branchName -> runHop branchName
    Fresh branchName -> runFresh branchName
    New newOptions -> runNew newOptions
    Rename issueNumOrBranch -> runRename issueNumOrBranch
    Issue issueOpts -> runIssue issueOpts
    Stash cmd -> case cmd of
        StashSave name       -> runStash name
        StashDiff num        -> runStashDiff num
        StashClear forceFlag -> runStashClear forceFlag
        StashList            -> runStashList
    Unstash -> runUnstash
    Commit opts -> runCommit opts
    Wip -> runWip
    Uncommit -> runUncommit
    Fix message pushBool -> runFix message pushBool
    Amend localAmend -> runAmend localAmend
    Resolve branchName -> runResolve branchName
    Push forceFlag -> runPush forceFlag
    Sync forceFlag -> runSync forceFlag
    Clear forceFlag -> runClear forceFlag
    Current -> runCurrent >>= flip whenJust (\i -> runIssue defaultIssueOptions {ioIssueNumber = Just i})
    Status commit -> runCurrent >> runStatus commit
    Diff commit -> runDiff commit
    Clone name -> runClone name
    Fork name -> runFork name
    Log commit -> runLog commit
    Milestones -> runMilestones
    Pr isDraft -> runPr isDraft

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
    | New !NewOptions
    | Rename !Text  -- ^ Issue number or branch name
    | Issue IssueOptions
    | Stash !StashCmd
    | Unstash
    | Commit CommitOptions
    | Wip
    | Uncommit
    | Fix
        (Maybe Text)  -- ^ Text of the fix commit
        ForceFlag     -- ^ Force push
    | Amend
        Bool  -- ^ Local amend
    | Resolve (Maybe Text)
    | Push ForceFlag
    | Sync ForceFlag
    | Clear ForceFlag
    | Current
    | Status (Maybe Text)
    | Diff (Maybe Text)
    | Clone !Text
    | Fork !Text
    | Log (Maybe Text)
    | Milestones
    | Pr
        !Bool  -- ^ Create a draft PR?

-- | Subcommands for the @git stash@ command
data StashCmd
    -- | @git stash@ or @git stash push@ if name is provided
    = StashSave
        !(Maybe Text)  -- ^ Optional name

    -- | @git stash show@ - show diff
    | StashDiff
        !(Maybe Int)  -- ^ Stash index

    -- | @git stash list@
    | StashList

    -- | @git stash clear@
    | StashClear
        !ForceFlag

-- | Commands parser.
hitP :: Parser HitCommand
hitP = subparser
    $ com "hop"      hopP      "Switch to branch and sync it"
   <> com "fresh"    freshP    "Rebase current branch on remote one"
   <> com "new"      newP      "Create new branch from the current one"
   <> com "rename"   renameP   "Rename current branch to the given issue number branch or text"
   <> com "stash"    stashP    "Stash all local changes"
   <> com "unstash"  unstashP  "Unstash previously stashed changes"
   <> com "commit"   commitP   "Commit all local changes and prepend issue number"
   <> com "wip"      wipP      "Create WIP commit"
   <> com "pr"       prP       "Commit all local changes and open the PR at GitHub"
   <> com "uncommit" uncommitP "Reset to the previous commit saving the changes"
   <> com "fix"      fixP      "Fix requested changes to the last commit"
   <> com "amend"    amendP    "Amend changes to the last commit and force push"
   <> com "issue"    issueP    "Show the information about the issue"
   <> com "push"     pushP     "Push the current branch"
   <> com "sync"     syncP     "Sync local branch with its remote"
   <> com "resolve"  resolveP  "Switch to the main branch, sync and delete the branch"
   <> com "clear"    clearP    "Remove all local changes permanently"
   <> com "current"  currentP  "Show info about current branch and issue (if applicable)"
   <> com "status"   statusP   "Show current branch and beautiful stats with COMMIT_HASH (by default HEAD)"
   <> com "diff"     diffP     "Display beautiful diff with COMMIT_HASH (by default HEAD)"
   <> com "clone"    cloneP    "Clone the repo. Use 'reponame' or 'username/reponame' formats"
   <> com "fork"     forkP     "Fork the repo. Use 'username/reponame' formats"
   <> com "log"      logP      "Display the log of the current commit or COMMIT_HASH"
   <> com "milestones" milestonesP "Show the list of open milestones for the project"
  where
    com :: String -> Parser HitCommand -> String -> Mod CommandFields HitCommand
    com name p desc = command name (info (helper <*> p) $ progDesc desc)

hopP :: Parser HitCommand
hopP = Hop <$> maybeBranchP

freshP :: Parser HitCommand
freshP = Fresh <$> maybeBranchP

newP :: Parser HitCommand
newP = do
    noCreateIssue <- switch
        $ long "issue"
       <> short 'i'
       <> help "Create new issue in addition to branch and assign it to you"
    noIssueOrBranch <- optional issueNumOrBranchOpt
    noMe <- meP
    noMilestone <- milestoneP
    pure $ New NewOptions{..}

renameP :: Parser HitCommand
renameP = Rename <$> issueNumOrBranchOpt

issueNumOrBranchOpt :: Parser Text
issueNumOrBranchOpt = strArgument (metavar "ISSUE_NUMBER_OR_BRANCH_NAME")

issueP :: Parser HitCommand
issueP = do
    ioIssueNumber <- optional issueNumP
    ioMe <- meP
    ioMilestone <- milestoneP
    pure $ Issue IssueOptions {..}

stashP :: Parser HitCommand
stashP = Stash <$> stashCmdP
  where
    stashCmdP :: Parser StashCmd
    stashCmdP = stashListP <|> stashDiffP <|> stashClearP <|> stashSaveP

    stashListP, stashDiffP, stashClearP:: Parser StashCmd
    stashListP  = subcom "list"  (pure StashList) "List all stashes"
    stashClearP = subcom "clear" (StashClear <$> forceFlagP) "Clear all stashes"
    stashDiffP  = subcom "diff"  (StashDiff <$> stashIndexP) "Show diff with the given stash"
      where
        stashIndexP :: Parser (Maybe Int)
        stashIndexP = optional $ argument auto $ help "Index of stash"

    stashSaveP :: Parser StashCmd
    stashSaveP = do
        stashName <- optional $ strOption $ mconcat
            [ long "name"
            , metavar "STRING"
            , help "Name of stash"
            ]
        pure $ StashSave stashName

    subcom :: String -> Parser StashCmd -> String -> Parser StashCmd
    subcom name p desc = subparser $ command name (info p $ progDesc desc)

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
    coIsForcePush <- forceFlagP
    pure $ Commit CommitOptions{..}

uncommitP :: Parser HitCommand
uncommitP = pure Uncommit

{- HLINT ignore "Use <$>"-}
fixP :: Parser HitCommand
fixP = do
    commitMsg <- commitMessageP
    forceFlag <- forceFlagP
    pure $ Fix commitMsg forceFlag

amendP :: Parser HitCommand
amendP = do
    localAmend <- switch
        $ long "local"
       <> short 'l'
       <> help "Whether to do a local amend only - without pushing"
    pure $ Amend localAmend

pushP :: Parser HitCommand
pushP = Push <$> forceFlagP

syncP :: Parser HitCommand
syncP = Sync <$> forceFlagP

clearP :: Parser HitCommand
clearP = Clear <$> forceFlagP

currentP :: Parser HitCommand
currentP = pure Current

statusP :: Parser HitCommand
statusP = Status <$> maybeCommitP

diffP :: Parser HitCommand
diffP = Diff <$> maybeCommitP

resolveP :: Parser HitCommand
resolveP = Resolve <$> maybeBranchP

cloneP :: Parser HitCommand
cloneP = Clone <$> repoP

forkP :: Parser HitCommand
forkP = Fork <$> repoP

logP :: Parser HitCommand
logP = Log <$> maybeCommitP

wipP :: Parser HitCommand
wipP = pure Wip

prP :: Parser HitCommand
prP = Pr <$> switch (long "draft" <> short 'd' <> help "Create a draft PR")

milestonesP :: Parser HitCommand
milestonesP = pure Milestones

-- | @--me@ flag.
meP :: Parser Bool
meP = switch $ long "me" <> help "Assigned to me"

repoP :: Parser Text
repoP = strArgument (metavar "REPOSITORY")

-- | Parse optional branch name as an argument.
maybeBranchP :: Parser (Maybe Text)
maybeBranchP = optional $ strArgument $ metavar "BRANCH_NAME"

-- | Parse optional commit hash as an argument.
maybeCommitP :: Parser (Maybe Text)
maybeCommitP = optional $ strArgument $ metavar "COMMIT_HASH"

-- | Parse optional commit message as an argument
commitMessageP :: Parser (Maybe Text)
commitMessageP = optional $ strArgument $ metavar "COMMIT_MESSAGE"

-- | Parse flag of force push or sync.
forceFlagP :: Parser ForceFlag
forceFlagP = flag Simple Force
    $  long "force"
    <> short 'f'
    <> help "Execute forcefully"

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
