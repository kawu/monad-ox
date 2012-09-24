-- | An Ox monad facilitates defining observation extraction rules.

module Control.Monad.Ox
( Ox
, Id
, at
, save
, when
, whenJT
, justTrue
, group
, memoize
, evalOx
) where

import Control.Applicative ((<$>), (<*), (*>))
import Control.Arrow (first)
import Control.Monad.RWS hiding (when)
import qualified Data.Vector as V
import qualified Data.MemoCombinators as Memo

-- | Observation type identifier.  It consists of a list of
-- integers, each integer representing the state of the Ox
-- monad on the particular level.
type Id = [Int]

-- | Increment the integer component of the top-most level.
inc :: Id -> Id
inc []      = error "incId: null id"
inc (x:xs)  = x+1 : xs

-- | Push new value to the Id stack.
grow :: Id -> Id
grow xs = 0 : xs

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

-- | The Ox monad is really an RWS monad with the sentence stored in the
-- reader part, observation type identifier handled by the state part
-- and the resulting observation values paired with identifiers printed
-- using the writer part.
type Ox t w a = RWS (V.Vector t) [(Id, w)] Id a

-- | Retrieve the underlying sentence.
getSent :: Ox t w (V.Vector t)
getSent = ask
{-# INLINE getSent #-}

-- | Retrieve the current identifier value.
getId :: Ox t w Id
getId = get
{-# INLINE getId #-}

-- | Set the new identifier value.
setId :: Id -> Ox t w ()
setId = put
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

at  :: (t -> a) -> Int -> Ox t w (Maybe a)
at f k = do
    sent <- getSent
    return $ if k < 0 || k >= V.length sent
        then Nothing
        else Just $ f (sent V.! k)

-- | Save the observation value in the writer component
-- of the Ox monad.
save :: Ox t w w -> Ox t w ()
save ox = do
    x <- ox
    withId $ \i -> tell [(i, x)]

-- | Do not use the plain 'Control.Monad.when' function unless you really
-- know what you are doing!  The 'when' here guarantees that IDs assigned to
-- observations *after* the 'when' code block will be tracked properly.
when :: Ox t w Bool -> Ox t w a -> Ox t w (Maybe a)
when cond act = incId >> cond >>= \b -> case b of
    False   -> return Nothing
    True    -> Just <$> below act

justTrue :: Maybe Bool -> Bool
justTrue Nothing    = False
justTrue (Just x)   = x

-- | When the condition monad returns the Just True value, perform
-- the given action.
whenJT :: Ox t w (Maybe Bool) -> Ox t w a -> Ox t w (Maybe a)
whenJT cond = when (justTrue <$> cond)

-- | Set all embedded observations indistinguishable with respect
-- to their top-most identifier components.
group :: Ox t w a -> Ox t w a
group act = do 
    i <- incId >> getId
    let top = getTop i
    x <- censor (map . first . setTop $ top) act
    setId i
    return x

-- -- | Memoize the given function on sentence positions.  As a first argument
-- -- the sentence length has to be supplied.
-- memoize :: Int -> (Int -> Ox t w a) -> Int -> Ox t w a
-- memoize n f = \k -> if k < 0 || k >= n
--     then f k
--     else memo V.! k
--   where
--     memo = V.fromList [f k | k <- [0..n-1]]

-- | TODO: Isn't that unsafe? What if the given function
-- changes the state of the Ox monad?
memoize :: (Int -> Ox t w a) -> Int -> Ox t w a
memoize f = Memo.integral f

evalOx :: Ox t w a -> V.Vector t -> [(Id, w)]
evalOx ox sent = (map (first reverse) . snd) (evalRWS ox sent [0])
