{- |
Module                  : Hit.Prompt
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains functions for colorful printing into terminal.
-}

module Hit.Prompt
    (  -- * Input with prompt
      Answer (..)
    , yesOrNoText
    , prompt

    , arrow
    ) where

import Colourista (errorMessage)
import Colourista.Short (b)

import qualified Data.Text as T


-- | Arrow symbol
arrow :: Text
arrow = " ➤ "

-- | Represents a user's answer
data Answer
    = Y
    | N

-- | Parse an answer to 'Answer'. Takes a default `Answer` to return in case of empty user input.
yesOrNoWithDefault :: Answer -> Text -> Maybe Answer
yesOrNoWithDefault def (T.toLower -> answer )
    | T.null answer = Just def
    | answer `elem` ["yes", "y", "ys"] = Just Y
    | answer `elem` ["no", "n"]  = Just N
    | otherwise = Nothing

yesOrNoText :: Answer -> Text
yesOrNoText N = "y/" <> b "[n]"
yesOrNoText Y = b "[y]" <> "/n"

-- | Prompt user for y/n input. Takes a default 'Answer' for the case of empty user input.
prompt :: Answer -> IO Answer
prompt def = do
    answer <- yesOrNoWithDefault def . T.strip <$> getLine
    case answer of
        Just ans -> pure ans
        Nothing -> do
           errorMessage "This is not a valid choice."
           prompt def
