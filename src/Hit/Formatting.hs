{- |
Module                  : Hit.Formatting
Copyright               : (c) 2019-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Common functions to format output in a certain way,
-}

module Hit.Formatting
    ( maxLenOn
    , stripRfc
    ) where

import qualified Data.Text as T


{- |
@maxLenOn f xs@ finds the longest text length from 'x's by apply 'f' to each 'x'.

@
maxLenOn show [1, 100, 1000, 2, 200] ≡ 4  -- because: Text.length (show 1000) ≡ 4.
@
-}
maxLenOn :: Foldable f => (a -> Text) -> f a -> Int
maxLenOn f = foldl' (\acc a -> max acc $ T.length $ f a) 0

{- | Strip the @[RFC] @ prefix if present.
-}
stripRfc :: Text -> Text
stripRfc x = fromMaybe x $ T.stripPrefix "[RFC] " x
