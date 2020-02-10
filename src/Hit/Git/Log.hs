{- | Pretty @hit log@ command which outputs short, pretty and
informative commit log.
-}

module Hit.Git.Log
    ( runLog
    ) where

import Shellmet ()


-- | @hit log@ command.
runLog :: Maybe Text -> IO ()
runLog (fromMaybe "HEAD" -> commit) = "git"
    [ "log"
    , "--format=format: ❃ %C(bold blue)%h%C(reset): %C(green)%s%C(reset)%n    %C(bold)Author%C(reset): %an <%ae>%n    %C(bold)Date%C(reset):   %cd%n"
    , "--date=rfc"
    , commit
    ]
