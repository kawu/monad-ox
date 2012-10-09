-- | Popular transformation functions for the 'L.Text' observation type.

module Control.Monad.Ox.Text.Lazy
( prefix
, suffix
, substr
, shape
, pack
) where

import qualified Data.Char as C
import qualified Data.Text.Lazy as L

-- | Prefix of the given size or 'Nothing' if the size exceeds the
-- length of the text.
prefix :: Int -> L.Text -> Maybe L.Text
prefix k xs
    | k > 0  && k <= n      = Just $ takeL k xs
    | k <= 0 && n + k > 0   = Just $ takeL (n + k) xs
    | otherwise             = Nothing
  where
    n = lengthL xs

-- | Suffix of the given size or 'Nothing' if the size exceeds the
-- length of the text.
suffix :: Int -> L.Text -> Maybe L.Text
suffix k xs
    | k > 0  && k <= n      = Just . takeR k $ xs
    | k <= 0 && n + k > 0   = Just . takeR (n + k) $ xs
    | otherwise             = Nothing
  where
    takeR i = L.reverse . takeL i . L.reverse
    n = lengthL xs

-- | All substrings of the given size.
substr :: Int -> L.Text -> [L.Text]
substr k xs
    | lengthL x < k = [] 
    | otherwise = x : substr k (L.tail xs)
  where
    x = takeL k xs

-- | Shape of the text.  All lower-case characters are mapped to 'l',
-- upper-case characters to 'u', digits to 'd' and rest of characters
-- to 'x'.
shape :: L.Text -> L.Text
shape = L.map translate
  where
    translate char
        | C.isLower char = 'l'
        | C.isUpper char = 'u'
        | C.isDigit char = 'd'
        | otherwise      = 'x'

-- | Pack the text, that is remove all adjacent repetitions,
-- for example /aabcccdde -> abcde/.
pack :: L.Text -> L.Text
pack = L.pack . map L.head . L.group

-- | Length of the lazy text as plain Int.
lengthL :: L.Text -> Int
lengthL = fromIntegral . L.length

takeL :: Int -> L.Text -> L.Text
takeL = L.take . fromIntegral
