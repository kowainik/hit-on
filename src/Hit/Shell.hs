{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{- | This module contains neat orphan instance for function to be able to call
commands using just strings.
-}

module Hit.Shell
       (
       ) where

import System.Process (callCommand, showCommandForUser)


-- | This is needed to be able to call commands by writing strings.
instance (a ~ Text, b ~ ()) => IsString ([a] -> IO b) where
    fromString :: String -> [Text] -> IO ()
    fromString cmd args = do
        let cmdStr = showCommandForUser cmd (map toString args)
        putStrLn $ "⚙  " ++ cmdStr
        callCommand cmdStr
