import qualified Data.Vector as V
import Control.Applicative ((<$>))
import Data.Char (toLower)
import Control.Monad.Ox

data Word = Word
    { orth  :: String
    , known :: Bool }

pref :: Int -> String -> String
pref k xs
    | k > 0     = take k xs
    | otherwise = take (n - k) xs
  where
    n = length xs

schema sent = \k -> do
    mapM_ (save . lowOrth) [k - 1, k, k + 1]
    whenJT (known `at` k) $ do
        mapM_ (save . prefix k) [1, 2, 3]
        group $ mapM_ (save . prefix (k + 1)) [0, -1, -2]
        (save . (orth `at`)) k
    (save . (orth `at`)) k
  where
    at = atWith sent
    lowOrth i  = map toLower <$> orth `at` i
    prefix i j = pref j <$> orth `at` i

main = do
    mapM_ print . execOx $ schema sent 1
  where
    sent = V.fromList
        [ Word "Abcdef" True
        , Word "Bcdefg" True
        , Word "cdefgh" False
        , Word "Defghi" False
        , Word "Efghij" True ]
