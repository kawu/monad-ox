-- | Popular transformation functions for the 'String' observation type.

module Control.Monad.Ox.String
( prefix
, suffix
, substr
, shape
, pack
) where

import qualified Data.Char as C
import qualified Data.List as L

-- | Prefix of the given size or 'Nothing' if the size exceeds the
-- length of the string.
prefix :: Int -> String -> Maybe String
prefix k xs
    | k > 0  && k <= n      = Just $ take k xs
    | k <= 0 && n + k > 0   = Just $ take (n + k) xs
    | otherwise             = Nothing
  where
    n = length xs

-- | Suffix of the given size or 'Nothing' if the size exceeds the
-- length of the string.
suffix :: Int -> String -> Maybe String
suffix k xs
    | k > 0  && k <= n      = Just . reverse . take k . reverse $ xs
    | k <= 0 && n + k > 0   = Just . reverse . take (n + k) . reverse $ xs
    | otherwise             = Nothing
  where
    n = length xs

-- | All substrings of the given size.
substr :: Int -> String -> [String]
substr k xs
    | k > 0 && k <= n   = relevant $ map (take k) (L.tails xs)
    | otherwise         = []
  where
    n = length xs
    relevant
        = reverse
        . dropWhile ((<k).length)
        . reverse

-- | Shape of the string.  All lower-case characters are mapped to 'l',
-- upper-case characters to 'u', digits to 'd' and rest of characters
-- to 'x'.
shape :: String -> String
shape = map translate
  where
    translate char
        | C.isLower char = 'l'
        | C.isUpper char = 'u'
        | C.isDigit char = 'd'
        | otherwise      = 'x'

-- | Pack the string, that is remove all adjacent repetitions,
-- for example /aabcccdde -> abcde/.
pack :: String -> String
pack = map head . L.group
