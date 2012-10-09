-- | Popular transformation functions for the 'T.Text' observation type.

module Control.Monad.Ox.Text
( prefix
, suffix
, substr
, shape
, pack
) where

import qualified Data.Char as C
import qualified Data.Text as T

-- | Prefix of the given size or 'Nothing' if the size exceeds the
-- length of the text.
prefix :: Int -> T.Text -> Maybe T.Text
prefix k xs
    | k > 0  && k <= n      = Just $ T.take k xs
    | k <= 0 && n + k > 0   = Just $ T.take (n + k) xs
    | otherwise             = Nothing
  where
    n = T.length xs

-- | Suffix of the given size or 'Nothing' if the size exceeds the
-- length of the text.
suffix :: Int -> T.Text -> Maybe T.Text
suffix k xs
    | k > 0  && k <= n      = Just . takeR k $ xs
    | k <= 0 && n + k > 0   = Just . takeR (n + k) $ xs
    | otherwise             = Nothing
  where
    takeR i = T.reverse . T.take i . T.reverse
    n = T.length xs

-- | All substrings of the given size.
substr :: Int -> T.Text -> [T.Text]
substr k xs
    | T.length x < k = [] 
    | otherwise = x : substr k (T.tail xs)
  where
    x = T.take k xs

-- | Shape of the text.  All lower-case characters are mapped to 'l',
-- upper-case characters to 'u', digits to 'd' and rest of characters
-- to 'x'.
shape :: T.Text -> T.Text
shape = T.map translate
  where
    translate char
        | C.isLower char = 'l'
        | C.isUpper char = 'u'
        | C.isDigit char = 'd'
        | otherwise      = 'x'

-- | Pack the text, that is remove all adjacent repetitions,
-- for example /aabcccdde -> abcde/.
pack :: T.Text -> T.Text
pack = T.pack . map T.head . T.group
