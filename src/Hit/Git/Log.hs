{- |
Module                  : Hit.Git.Log
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Pretty @hit log@ command which outputs short, pretty and
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
