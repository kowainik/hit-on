{- |
Module                  : Hit.Hub
Copyright               : (c) 2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains helper functions to work with @hub@ tool.
-}

module Hit.Hub
    ( withHub
    ) where

import Colourista (errorMessage, warningMessage)
import Shellmet (($?))
import System.Directory (findExecutable)


{- |

-}
withHub
    :: [Text]  -- ^ Hub command arguments
    -> IO ()
withHub hubArgs = findExecutable "hub" >>= \case
    Just _ -> do
        isHubSuccess <- (True <$ "hub" hubArgs) $? pure False
        unless isHubSuccess $ do
            warningMessage "Error running 'hub'. Possible reason: incorrect password."
            exitFailure
    Nothing -> do
        errorMessage "'hub' is not found at this machine."
        warningMessage "Please install 'hub' for the proper work of 'hit'."
        exitFailure
