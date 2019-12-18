-- | Everything related to the `hit hop` command

module Hit.Git.Hop 
    ( runHop
    ) where

import qualified Data.Text as T
import System.Process (callCommand)

import Hit.Git.Common (nameOrMaster)

-- | @hit hop@ command.
runHop :: Maybe Text -> IO ()
runHop (nameOrMaster -> branch) = do
    callCommand $ "git checkout " ++ (T.unpack branch)
    callCommand $ "git pull --rebase --prune"
