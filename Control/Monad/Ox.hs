-- | An Ox monad facilitates defining observation extraction rules.

module Control.Monad.Ox
( Ox
, Id

, atWith
, atsWith

, save
, saves

, when
, whenJT
, group
, memoize
, execOx
) where

import Control.Applicative ((<$>), (<*), (*>))
import Control.Arrow (first)
import Control.Monad.State hiding (when)
import Control.Monad.Writer hiding (when)
import Data.Maybe (maybeToList)
import qualified Data.Vector as V
import qualified Data.MemoCombinators as Memo

-- | Observation type identifier.  It consists of a list of
-- integers, each integer representing a state of the Ox
-- monad on the particular level.
type Id = [Int]

-- | Increment the integer component of the top-most level.
inc :: Id -> Id
inc []      = error "incId: null id"
inc (x:xs)  = x+1 : xs

-- | Push new value to the Id stack.
grow :: Id -> Id
grow xs = 1 : xs

-- | Pop value from the stack.
shrink :: Id -> Id
shrink []       = error "shrink: null id"
shrink (_:xs)   = xs

-- | Set the top-most component to the given value.
getTop :: Id -> Int
getTop [] = error "getTop: null id"
getTop (x:_) = x

-- | Set the top-most component to the given value.
setTop :: Int -> Id -> Id
setTop _ [] = error "setTop: null id"
setTop x (_:xs) = x:xs

-- | The Ox is a monad stack with observation type identifier handled by
-- the state monad and the resulting observation values paired with identifiers
-- printed using the writer monad.
type Ox t w a = WriterT [(Id, w)] (State Id) a

-- | Retrieve the current identifier value.
getId :: Ox t w Id
getId = lift get
{-# INLINE getId #-}

-- | Set the new identifier value.
setId :: Id -> Ox t w ()
setId = lift . put
{-# INLINE setId #-}

updateId :: (Id -> Id) -> Ox t w ()
updateId f = do
    i <- getId
    setId (f i)

incId :: Ox t w ()
incId = updateId inc

-- | Perform the identifier-dependent action and increase
-- the identifier.
withId :: (Id -> Ox t w a) -> Ox t w a
withId act = do
    x <- act =<< getId
    incId
    return x

below :: Ox t w a -> Ox t w a
below act = updateId grow *> act <* updateId shrink

atWith :: V.Vector t -> (t -> a) -> Int -> Maybe a
atWith xs f k =
    if k < 0 || k >= V.length xs
        then Nothing
        else Just $ f (xs V.! k)

atsWith  :: V.Vector t -> (t -> [a]) -> Int -> [a]
atsWith xs f k =
    if k < 0 || k >= V.length xs
        then []
        else f (xs V.! k)

-- | Save observation values in the writer monad of the Ox stack.
saves :: [w] -> Ox t w ()
saves xs = withId $ \i -> tell [(i, x) | x <- xs]

save :: Maybe w -> Ox t w ()
save = saves . maybeToList

-- | Do not use the plain 'Control.Monad.when' function unless you really
-- know what you are doing!  The 'when' here guarantees that IDs assigned to
-- observations *after* the 'when' code block will be tracked properly.
when :: Bool -> Ox t w a -> Ox t w (Maybe a)
when cond act = do
    x <- case cond of
        False -> return Nothing
        True  -> Just <$> below act
    incId
    return x

-- | When the condition monad returns the Just True value, perform
-- the given action.
whenJT :: Maybe Bool -> Ox t w a -> Ox t w (Maybe a)
whenJT cond =
    when (justTrue cond)
  where
    justTrue Nothing  = False
    justTrue (Just x) = x

-- | Set all embedded observations indistinguishable with respect
-- to their top-most identifier components.
-- FIXME: Perhaps should set only the current level, not the deeper ones.
group :: Ox t w a -> Ox t w a
group act = do 
    i <- getId
    let top = getTop i
    x <- censor (map . first . setTop $ top) act
    setId (inc i)
    return x

memoize :: (Int -> a) -> Int -> a
memoize f = Memo.integral f

execOx :: Ox t w a -> [(Id, w)]
execOx ox =
    (map (first reverse) . fst)
    (runState (execWriterT ox) [1])
