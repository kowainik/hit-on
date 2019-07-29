{- | Data types that describe model of the @git diff@ command to display diffs
in pretty way.
-}

module Hit.Git.Status
       ( showPrettyDiff
       ) where

import Shellmet (($?), ($|))
import System.Process (callCommand)

import Hit.ColorTerminal (blueCode, boldCode, cyanCode, greenCode, magentaCode, redCode, resetCode,
                          yellowCode)

import qualified Data.Text as T


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

-- | Map conventional characters to 'PatchType'
parsePatchType :: Text -> Maybe PatchType
parsePatchType = \case
    "A" -> Just Added
    "C" -> Just Copied
    "D" -> Just Deleted
    "M" -> Just Modified
    "R" -> Just Renamed
    "T" -> Just TypeChanged
    "U" -> Just Unmerged
    "X" -> Just Unknown
    "B" -> Just BrokenPairing
    _   -> Nothing

-- | Display 'PatchType' in colorful and expanded text.
displayPatchType :: PatchType -> Text
displayPatchType = \case
    Added         -> coloredIn greenCode   "added"
    Copied        -> coloredIn blueCode    "copied"
    Deleted       -> coloredIn redCode     "deleted"
    Modified      -> coloredIn magentaCode "modified"
    Renamed       -> coloredIn yellowCode  "renamed"
    TypeChanged   -> coloredIn cyanCode    "type-changed"
    Unmerged      -> inBold                "unmerged"
    Unknown       -> inBold                "unknown"
    BrokenPairing -> inBold                "broken"
  where
    coloredIn :: Text -> Text -> Text
    coloredIn color text = color <> inBold text

    inBold :: Text -> Text
    inBold text = boldCode <> text <> resetCode

-- | Output of the @git diff --name-status@ command.
data DiffName = DiffName
    { diffNameFile :: !Text       -- ^ file name
    , diffNameType :: !PatchType  -- ^ type of the changed file
    }

parseDiffName :: [Text] -> Maybe DiffName
parseDiffName [t, name] = DiffName name <$> parsePatchType t
parseDiffName _         = Nothing

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
 .foo.un~  | Bin 0 -> 523 bytes
 README.md |   4 ++++
 foo       |   1 +
@
-}
parseDiffStat :: [Text] -> Maybe DiffStat
parseDiffStat = \case
    [diffStatFile, diffStatCount, diffStatSigns] -> Just DiffStat{..}
    diffStatFile:"Bin":rest -> Just DiffStat
        { diffStatCount = "Bin"
        , diffStatSigns = unwords rest
        , ..
        }
    _ -> Nothing

showPrettyDiff :: Text -> IO ()
showPrettyDiff commit = do
    -- 1. Check rebase in progress and tell about it
    whenM isRebaseInProgress $ do
        putTextLn gitRebaseHelp
        showConlictFiles

    -- 2. Output pretty diff
    diffName <- map words   . lines <$> "git" $| ["diff", commit, "--name-status"]
    diffStat <- map toStats . lines <$> "git" $| ["diff", commit, "--stat", "--color=always"]
    let fileTypes = sortWith diffNameFile $ mapMaybe parseDiffName diffName
    let fileStats = sortWith diffStatFile $ mapMaybe parseDiffStat diffStat
    let rows = zipWith joinDiffs fileTypes fileStats
    putText $ formatTableAligned rows
  where
    toStats :: Text -> [Text]
    toStats = foldMap words . T.split (== '|')

    joinDiffs :: DiffName -> DiffStat -> (Text, Text, Text, Text)
    joinDiffs DiffName{..} DiffStat{..} =
        (displayPatchType diffNameType, diffNameFile, diffStatCount, diffStatSigns)

    formatTableAligned :: [(Text, Text, Text, Text)] -> Text
    formatTableAligned rows = unlines $ map formatRow rows
      where
        formatRow :: (Text, Text, Text, Text) -> Text
        formatRow (fileType, fileName, fileCount, fileSigns) =
            padRight typeSize fileType
            <> "  "
            <> padRight nameSize fileName
            <> " | "
            <> padLeft countSize fileCount
            <> " "
            <> fileSigns

        padRight :: Int -> Text -> Text
        padRight n t = t <> T.replicate (n - T.length t) " "

        padLeft :: Int -> Text -> Text
        padLeft n t = T.replicate (n - T.length t) " " <> t

        typeSize, nameSize :: Int
        typeSize  = maxOn (\(a, _, _, _) -> a) rows
        nameSize  = maxOn (\(_, b, _, _) -> b) rows
        countSize = maxOn (\(_, _, c, _) -> c) rows

        maxOn :: (a -> Text) -> [a] -> Int
        maxOn f = foldl' (\acc a -> max acc $ T.length $ f a) 0

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
    , boldCode <> yellowCode <> "Rebase in progress! What you can do:" <> resetCode
    , "    " <> cyanCode <> "git rebase --continue " <> resetCode <> ": after fixing conflicts"
    , "    " <> cyanCode <> "git rebase --skip     " <> resetCode <> ": to skip this patch"
    , "    " <> cyanCode <> "git rebase --abort    " <> resetCode <> ": to abort to the original branch"
    ]

showConlictFiles :: IO ()
showConlictFiles = do
    conflictFiles <- lines <$> "git" $| ["diff", "--name-only", "--diff-filter=U"]
    unless (null conflictFiles) $
        putTextLn $ unlines $
            ( boldCode <> redCode <> "Conflict files:" <> resetCode )
            : map ("    " <>) conflictFiles
