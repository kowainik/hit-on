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
