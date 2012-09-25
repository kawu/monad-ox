import qualified Data.Vector as V
import Control.Applicative ((<$>))
import Data.Char (toLower)

import Control.Monad.Ox
import qualified Control.Monad.Ox.String as Ox

data Word = Word
    { orth  :: String
    , known :: Bool }

schema :: V.Vector Word -> Int -> Ox Word String ()
schema sent = \k -> do
    mapM_ (save . lowOrth) [k - 1, k, k + 1]
    whenJT (known `at` k) $ do
        mapM_ (save . prefix k) [1, 2, 3, 10]
        group $ mapM_ (save . prefix (k + 1)) [0, -1, -2]
        (save . (orth `at`)) k
    save $ suffix (k - 1) 3
  where
    at = atWith sent
    lowOrth i  = map toLower <$> orth `at` i
    prefix i j = Ox.prefix j =<< orth `at` i
    suffix i j = Ox.suffix j =<< orth `at` i

main = do
    mapM_ print . execOx $ schema sent 1
  where
    sent = V.fromList
        [ Word "Abcdef" True
        , Word "Bcdefg" True
        , Word "cdefgh" False
        , Word "Defghi" False
        , Word "Efghij" True ]
