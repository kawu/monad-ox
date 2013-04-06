-- | The Ox monad facilitates writing functional expressions over the
-- input sentence with arbitrary type of sentence token.

module Control.Monad.Ox
( 
-- * Types
  Ox
, Id

-- * Functions
, save
, saves
, when
, whenJT
, group

-- * Ox monad execution
, execOx

-- * Utilities
, atWith
, atsWith
) where

import Control.Applicative ((<$>), (<*), (*>))
import Control.Arrow (first)
import Control.Monad.State hiding (when)
import Control.Monad.Writer hiding (when)
import Data.Maybe (maybeToList)
import qualified Data.Vector as V

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
type Ox o a = WriterT [(Id, o)] (State Id) a

-- | Retrieve the current identifier value.
getId :: Ox o Id
getId = lift get
{-# INLINE getId #-}

-- | Set the new identifier value.
setId :: Id -> Ox o ()
setId = lift . put
{-# INLINE setId #-}

-- | Update the current identifier of the Ox monad.
updateId :: (Id -> Id) -> Ox o ()
updateId f = do
    i <- getId
    setId (f i)

-- | Increase the current identifier of the Ox monad.
incId :: Ox o ()
incId = updateId inc

-- | Perform the identifier-dependent action and increase the identifier.
withId :: (Id -> Ox o a) -> Ox o a
withId act = do
    x <- act =<< getId
    incId
    return x

-- | Perform the Ox action on the lower level.
below :: Ox o a -> Ox o a
below act = updateId grow *> act <* updateId shrink

-- | Save observation values in the writer monad of the Ox stack.
saves :: [o] -> Ox o ()
saves xs = withId $ \i -> tell [(i, x) | x <- xs]

-- | Save the observation value.
save :: Maybe o -> Ox o ()
save = saves . maybeToList

-- | Perform the Ox action only when the 'cond' is True.  It works like
-- the standard 'Control.Monad.when' function but also changes the current
-- identifier value.
when :: Bool -> Ox o a -> Ox o (Maybe a)
when cond act = do
    x <- case cond of
        False -> return Nothing
        True  -> Just <$> below act
    incId
    return x

-- | Perform the action only when the given condition is equal to Just True.
whenJT :: Maybe Bool -> Ox o a -> Ox o (Maybe a)
whenJT cond =
    when (justTrue cond)
  where
    justTrue Nothing  = False
    justTrue (Just x) = x

-- | Make all embedded observations to be indistinguishable with respect
-- to their top-most identifier components.
-- TODO: Perhaps should set only the current level, not the deeper ones.
group :: Ox o a -> Ox o a
group act = do 
    i <- getId
    let top = getTop i
    x <- censor (map . first . setTop $ top) act
    setId (inc i)
    return x

-- | Execute the Ox monad and retrieve the saved (with the 'save' and
-- 'saves' functions) results.
execOx :: Ox o a -> [(Id, o)]
execOx ox =
    (map (first reverse) . fst)
    (runState (execWriterT ox) [1])

------------------------------
-- Utilities
------------------------------

-- | Value of the 't -> a' function with respect to the given sentence
-- and sentence position.  Return Nothing if the position is out of
-- bounds.
atWith :: V.Vector a -> (a -> b) -> Int -> Maybe b
atWith xs f k =
    if k < 0 || k >= V.length xs
        then Nothing
        else Just $ f (xs V.! k)

-- | Value of the 't -> [a]' function with respect to the given sentence
-- and sentence position.  Return empty list if the position is out of
-- bounds.
atsWith  :: V.Vector a -> (a -> [b]) -> Int -> [b]
atsWith xs f k =
    if k < 0 || k >= V.length xs
        then []
        else f (xs V.! k)
