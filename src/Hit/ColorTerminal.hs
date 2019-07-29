-- | This module contains functions for colorful printing into terminal.

module Hit.ColorTerminal
       ( Color (..)
       , putStrFlush
       , beautyPrint
       , bold
       , boldText
       , boldDefault
       , italic
       , reset
       , setColor
       , successMessage
       , warningMessage
       , errorMessage
       , infoMessage
       , skipMessage

         -- * Colors
       , redCode
       , blueCode
       , greenCode
       , yellowCode
       , cyanCode
       , magentaCode

         -- * Other formatters
       , boldCode
       , resetCode
       , blueBg

         -- * Input with prompt
       , Answer (..)
       , prompt

       , arrow
       ) where

import System.Console.ANSI (Color (..), ColorIntensity (Dull, Vivid),
                            ConsoleIntensity (BoldIntensity), ConsoleLayer (Background, Foreground),
                            SGR (..), setSGR, setSGRCode)
import System.IO (hFlush)

import qualified Data.Text as T


-- | Explicit flush ensures prompt messages are in the correct order on all systems.
putStrFlush :: Text -> IO ()
putStrFlush msg = do
    putText msg
    hFlush stdout

setColor :: Color -> IO ()
setColor color = setSGR [SetColor Foreground Vivid color]

-- | Starts bold printing.
bold :: IO ()
bold = setSGR [SetConsoleIntensity BoldIntensity]

italic :: IO ()
italic = setSGR [SetItalicized True]

-- | Resets all previous settings.
reset :: IO ()
reset = do
    setSGR [Reset]
    hFlush stdout

-- | Takes list of formatting options, prints text using this format options.
beautyPrint :: [IO ()] -> Text -> IO ()
beautyPrint formats msg = do
    sequence_ formats
    putText msg
    reset

boldText :: Text -> IO ()
boldText message = bold >> putStrFlush message >> reset

boldDefault :: Text -> IO ()
boldDefault message = boldText (" [" <> message <> "]")

colorMessage :: Color -> Text -> IO ()
colorMessage color message = do
    setColor color
    putTextLn $ "  " <> message
    reset

errorMessage, warningMessage, successMessage, infoMessage, skipMessage :: Text -> IO ()
errorMessage   = colorMessage Red
warningMessage = colorMessage Yellow
successMessage = colorMessage Green
infoMessage    = colorMessage Blue
skipMessage    = colorMessage Cyan

redCode, blueCode, greenCode, yellowCode, cyanCode, magentaCode :: Text
redCode     = mkColor Red
blueCode    = mkColor Blue
greenCode   = mkColor Green
yellowCode  = mkColor Yellow
cyanCode    = mkColor Cyan
magentaCode = mkColor Magenta

boldCode, resetCode, blueBg :: Text
boldCode   = toText $ setSGRCode [SetConsoleIntensity BoldIntensity]
resetCode  = toText $ setSGRCode [Reset]
blueBg     = toText $ setSGRCode [SetColor Foreground Dull White, SetColor Background Dull Blue]

mkColor :: Color -> Text
mkColor color = toText $ setSGRCode [SetColor Foreground Vivid color]

-- | Arrow symbol
arrow :: Text
arrow = " ➤  "

-- | Represents a user's answer
data Answer = Y | N

-- | Parse an answer to 'Answer'
yesOrNo :: Text -> Maybe Answer
yesOrNo (T.toLower -> answer )
    | T.null answer = Just Y
    | answer `elem` ["yes", "y", "ys"] = Just Y
    | answer `elem` ["no", "n"]  = Just N
    | otherwise = Nothing

prompt :: IO Answer
prompt = do
    answer <- yesOrNo . T.strip <$> getLine
    case answer of
        Just ans -> pure ans
        Nothing -> do
           errorMessage "This wasn't a valid choice."
           prompt
