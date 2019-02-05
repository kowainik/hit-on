{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{- | This module contains neat utilities to be able to work with
shell commands in generic and simple way using just strings.
-}

module Hit.Shell
       ( ($|)
       ) where

import System.Process (callCommand, readProcess, showCommandForUser)


-- | This is needed to be able to call commands by writing strings.
instance (a ~ Text, b ~ ()) => IsString ([a] -> IO b) where
    fromString :: String -> [Text] -> IO ()
    fromString cmd args = do
        let cmdStr = showCommandForUser cmd (map toString args)
        putStrLn $ "⚙  " ++ cmdStr
        callCommand cmdStr

-- | Run shell command with given options and return stdout of executed command.
infix 5 $|
($|) :: FilePath -> [Text] -> IO Text
cmd $| args = toText <$> readProcess cmd (map toString args) ""
