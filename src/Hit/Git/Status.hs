{- |
Module                  : Hit.Git.Status
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Data types that describe model of the @git diff@ command to display diffs
in pretty way.
-}

module Hit.Git.Status
       ( runStatus
       ) where

import Colourista (blue, bold, cyan, formatWith, green, magenta, red, reset, yellow)
import Colourista.Short (b)
import Shellmet (($?), ($|))
import System.Process (callCommand)

import Hit.Git.Common (withDeletedFiles, withUntrackedFiles)

import qualified Data.Text as T
import qualified Hit.Formatting as Fmt


{- | Show stats from the given commit. If commit is not specified, uses HEAD.
-}
runStatus :: Maybe Text -> IO ()
runStatus (fromMaybe "HEAD" -> commit) =
    withDeletedFiles $ withUntrackedFiles $ showPrettyDiff commit

-- | Enum that represents all possible types of file modifications.
data PatchType
    = Added
    | Copied
    | Deleted
    | Modified
    | Renamed
    | TypeChanged
    | Unmerged
    | Unknown
    | BrokenPairing

{- | Parses the different change types.
Renames and copies contain an additional similarity percentage between the two files.

Potential values include:
 'A' for newly added files
 'M' for modified files
 'R100' for renamed files, where 100 denotes a similarity percentage
 'C075' for copied files, where 75 denotes a similarity percentage
-}
parsePatchType :: Text -> Maybe PatchType
parsePatchType t = do
    (c, _) <- T.uncons t
    case c of
        'A' -> Just Added
        'C' -> Just Copied
        'D' -> Just Deleted
        'M' -> Just Modified
        'R' -> Just Renamed
        'T' -> Just TypeChanged
        'U' -> Just Unmerged
        'X' -> Just Unknown
        'B' -> Just BrokenPairing
        _   -> Nothing

-- | Display 'PatchType' in colorful and expanded text.
displayPatchType :: PatchType -> Text
displayPatchType = \case
    Added         -> coloredIn green   "added"
    Copied        -> coloredIn blue    "copied"
    Deleted       -> coloredIn red     "deleted"
    Modified      -> coloredIn magenta "modified"
    Renamed       -> coloredIn yellow  "renamed"
    TypeChanged   -> coloredIn cyan    "type-changed"
    Unmerged      -> b                 "unmerged"
    Unknown       -> b                 "unknown"
    BrokenPairing -> b                 "broken"
  where
    coloredIn :: Text -> Text -> Text
    coloredIn color = formatWith [color, bold]

-- | Output of the @git diff --name-status@ command.
data DiffName = DiffName
    { diffNameFile :: !Text       -- ^ file name
    , diffNameType :: !PatchType  -- ^ type of the changed file
    }

{- | Parses a diff list of file names.
When a file was renamed, both the previous and the new filename are given.
These could be in the following formats:

@
<patch-type> <filename>
<patch-type> <old-filename> <new-filename>
@

Typical raw text returned by @git@ can look like this:

@
 M     README.md
 A     foo
 R100  bar       baz
@
-}
parseDiffName :: [Text] -> Maybe DiffName
parseDiffName (t : xs) = DiffName (unwords xs) <$> parsePatchType t
parseDiffName _        = Nothing

-- | Output of the @git diff --stat@ command.
data DiffStat = DiffStat
    { diffStatFile  :: !Text  -- ^ file name
    , diffStatCount :: !Text  -- ^ number of changed lines
    , diffStatSigns :: !Text  -- ^ + and - stats
    }

{- | This command parses diff stats in the following format:

@
<filename> | <n> <pluses-and-minuses>
@

It also handles special case of binary files. Typical raw text returned by @git@
can look like this:

@
 .foo.un~               | Bin 0 -> 523 bytes
 README.md              |   4 ++++
 foo                    |   1 +
 test/{bar => foo/baz}  |   2 --
 qux => quux            |   0
@
-}
parseDiffStat :: [Text] -> Maybe DiffStat
parseDiffStat = \case
    [diffStatFile, diffStatCount, diffStatSigns] -> Just DiffStat{..}
    prevFile:"=>":newFile:diffStatCount:rest -> Just DiffStat
        { diffStatSigns = unwords rest
        , diffStatFile = expandFilePath (prevFile, newFile)
        , ..
        }
    diffStatFile:"Bin":rest -> Just DiffStat
        { diffStatCount = "Bin"
        , diffStatSigns = unwords rest
        , ..
        }
    _ -> Nothing

{- | Get diff stats for a single file.

Shell command example:

@
git diff HEAD~1 --stat CHANGELOG.md
@
-}
fileDiffStat :: Text -> Text -> IO DiffStat
fileDiffStat commit fileName = do
    diffStat <- "git" $| ["diff", commit, "--stat", "--color=always", "--", fileName]
    let stats = map toStats $ lines diffStat

    let emptyDiffStat = DiffStat
            { diffStatFile  = fileName
            , diffStatCount = "0"
            , diffStatSigns = ""
            }

    -- it should always be the list of a single element
    pure
        $ fromMaybe emptyDiffStat
        $ viaNonEmpty head stats >>= parseDiffStat
  where
    toStats :: Text -> [Text]
    toStats = foldMap words . T.split (== '|')

{- | Attempts to expand shortened paths which can appear in `git diff --stat`.
This function takes a tuple of the part before and after the arrow.
Examples of possible paths and what they should expand to:

@
 a.in      => b.out      | a.in     => b.out
 test/{bar => baz}       | test/bar => test/baz
 test/{bar => a1{/baz}   | test/bar => test/a1{/baz
 test/{    => bar}/baz   | test/baz => test/bar/baz
@
-}
expandFilePath :: (Text, Text) -> Text
expandFilePath (left, right) = T.intercalate " => " $ map wrap middle
  where
    bracket :: Char -> Bool
    bracket c = c == '{' || c == '}'

    splitBrackets :: (Text, [Text], Text)
    splitBrackets = (l, [lm, rm], r)
      where
        (l, T.dropWhile bracket -> lm) = T.breakOn "{" left
        (T.dropWhileEnd bracket -> rm, r) = T.breakOnEnd "}" right

    wrap :: Text -> Text
    wrap mid = unwords [prefix, mid, suffix]

    middle :: [Text]
    prefix, suffix :: Text
    (prefix, middle, suffix) = splitBrackets

showPrettyDiff :: Text -> IO ()
showPrettyDiff commit = do
    -- 1. Check rebase in progress and tell about it
    whenM isRebaseInProgress $ do
        putTextLn gitRebaseHelp
        showConlictFiles

    -- 2. Output pretty diff
    gitDiffName <- map words . lines <$> "git" $| ["diff", commit, "--name-status"]
    let diffNames = sortWith diffNameFile $ mapMaybe parseDiffName gitDiffName

    rows <- forM diffNames $ \diffName -> do
        diffStat <- fileDiffStat commit (diffNameFile diffName)
        pure $ joinDiffs diffName diffStat

    putText $ formatTableAligned rows
  where
    joinDiffs :: DiffName -> DiffStat -> (Text, Text, Text, Text)
    joinDiffs DiffName{..} DiffStat{..} =
        ( displayPatchType diffNameType
        , formatName diffNameType diffNameFile
        , diffStatCount
        , diffStatSigns
        )

    formatName :: PatchType -> Text -> Text
    formatName = \case
        Renamed -> formatRename
        Copied -> formatRename
        _ -> id
      where
        formatRename :: Text -> Text
        formatRename = T.intercalate " -> " . words

    formatTableAligned :: [(Text, Text, Text, Text)] -> Text
    formatTableAligned rows = unlines $ map formatRow rows
      where
        formatRow :: (Text, Text, Text, Text) -> Text
        formatRow (fileType, fileName, fileCount, fileSigns) =
            T.justifyLeft typeSize ' ' fileType
            <> "  "
            <> T.justifyLeft nameSize ' ' fileName
            <> " | "
            <> T.justifyRight countSize ' ' fileCount
            <> " "
            <> fileSigns

        typeSize, nameSize :: Int
        typeSize  = Fmt.maxLenOn (\(a, _, _, _) -> a) rows
        nameSize  = Fmt.maxLenOn (\(_, x, _, _) -> x) rows
        countSize = Fmt.maxLenOn (\(_, _, c, _) -> c) rows

{- | Returns 'True' if rebase is in progress. Calls magic comand and if this
command exits with code 1 then there's no rebase in progress.
-}
isRebaseInProgress :: IO Bool
isRebaseInProgress = do
    let checkRebaseCmd = callCommand "ls `git rev-parse --git-dir` | grep rebase > /dev/null 2>&1"
    True <$ checkRebaseCmd $? pure False

gitRebaseHelp :: Text
gitRebaseHelp = unlines
    [ ""
    , formatWith [bold, yellow] "Rebase in progress! What you can do:"
    , "    " <> cyan <> "git rebase --continue " <> reset <> ": after fixing conflicts"
    , "    " <> cyan <> "git rebase --skip     " <> reset <> ": to skip this patch"
    , "    " <> cyan <> "git rebase --abort    " <> reset <> ": to abort to the original branch"
    ]

showConlictFiles :: IO ()
showConlictFiles = do
    conflictFiles <- lines <$> "git" $| ["diff", "--name-only", "--diff-filter=U"]
    unless (null conflictFiles) $
        putTextLn $ unlines $
            formatWith [bold, red] "Conflict files:"
            : map ("    " <>) conflictFiles
