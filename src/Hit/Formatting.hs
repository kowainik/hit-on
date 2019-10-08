module Hit.Formatting
  ( padLeft
  , padRight
  , maxLenOn
  )
where

import qualified Data.Text                     as T

-- | @padLeft n t@ pads the text 't' with spaces on the left until it reaches length 'n'.
--
-- @
-- padLeft 10 "hello"' ≡ "·····hello"
-- padLeft  3 "hello"' ≡ "hello"
-- @
padLeft :: Int -> Text -> Text
padLeft n t = T.replicate (n - T.length t) " " <> t

-- | @padRight n t@ pads the text 't' with spaces on the right until it reaches length 'n'.
--
-- @
-- padRight 10 "hello"' ≡ "hello·····"
-- padRight  3 "hello"' ≡ "hello"
-- @
padRight :: Int -> Text -> Text
padRight n t = t <> T.replicate (n - T.length t) " "

-- | @maxLenOn f xs@ finds the longest text length from 'x's by apply 'f' to each 'x'.
--
-- @
-- maxLenOn show [1, 100, 1000, 2, 200] ≡ 4  -- because: Text.length (show 1000) ≡ 4.
-- @
maxLenOn :: Foldable f => (a -> Text) -> f a -> Int
maxLenOn f = foldl' (\acc a -> max acc $ T.length $ f a) 0
