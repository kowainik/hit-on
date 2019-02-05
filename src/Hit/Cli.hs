{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line interface for @hit@ executable.

module Hit.Cli
       ( hit
       ) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc, help, helper, info,
                            infoOption, long, metavar, progDesc, short, strArgument, subparser)

import Hit.ColorTerminal (blueCode, boldCode, redCode, resetCode)
import Hit.Git (runHop)

import qualified Paths_hit_on as Meta (version)


hit :: IO ()
hit = execParser cliParser >>= \case
    Hop branchName -> runHop branchName

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
cliParser :: ParserInfo HitCommand
cliParser = info ( helper <*> versionP <*> hitP )
    $ fullDesc <> progDesc "Haskell Git Helper Tool"

-- | Commands for
newtype HitCommand
    = Hop (Maybe Text)

-- | Commands parser.
hitP :: Parser HitCommand
hitP = subparser
    $ command "hop" (info (helper <*> hopP) $ progDesc "Switch to branch and sync it")

hopP :: Parser HitCommand
hopP = do
    branchName <- optional $ strArgument (metavar "BRANCH_NAME")
    pure $ Hop branchName

-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption hitVersion
    $ long "version"
   <> short 'v'
   <> help "Show hit's version"

hitVersion :: String
hitVersion = toString
    $ intercalate "\n"
    $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    sVersion = blueCode <> boldCode <> "Hit " <> "v" <>  showVersion Meta.version <> resetCode
    sHash = " ➤ " <> blueCode <> boldCode <> "Git revision: " <> resetCode <> $(gitHash)
    sDate = " ➤ " <> blueCode <> boldCode <> "Commit date:  " <> resetCode <> $(gitCommitDate)
    sDirty = redCode <> "There are non-committed files." <> resetCode
