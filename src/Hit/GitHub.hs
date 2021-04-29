{- |
Module                  : Hit.GitHub
Copyright               : (c) 2020-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains helper functions to work with GitHub API.
-}

module Hit.GitHub
    ( module Hit.GitHub.Auth
    , module Hit.GitHub.Issue
    , module Hit.GitHub.Milestone
    ) where

import Hit.GitHub.Auth
import Hit.GitHub.Issue
import Hit.GitHub.Milestone
