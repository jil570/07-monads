module GenericMonads where

import Test.HUnit
import Prelude hiding (mapM, sequence)

-- (a)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM = error "mapM: unimplemented"

testMapM :: Test
testMapM = undefined

-- (b)

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM = error "foldM: unimplemented"

testFoldM :: Test
testFoldM = undefined

-- (c)
--

sequence :: Monad m => [m a] -> m [a]
sequence = error "sequence: unimplemented"

testSequence :: Test
testSequence = undefined

-- (d) Define the Kleisli "fish operator"
--

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) = error ">=>: unimplemented"

testKleisli :: Test
testKleisli = undefined

-- (e)

--

join :: (Monad m) => m (m a) -> m a
join = error "join: unimplemented"

testJoin :: Test
testJoin = undefined

-- (f) Define the 'liftM' function

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM = error "liftM: unimplemented"

testLiftM :: Test
testLiftM = undefined

-- (g) And its two-argument version ...

liftM2 :: (Monad m) => (a -> b -> r) -> m a -> m b -> m r
liftM2 = error "liftM2: unimplemented"

testLiftM2 :: Test
testLiftM2 = undefined

-------------------------------------------------------------------------

-- NOTE: you may not be able to define all of these, but be sure to test the
-- ones that you do

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA = undefined

foldA :: Applicative f => (a -> b -> f a) -> a -> [b] -> f a
foldA = undefined

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = undefined

kleisliA :: Applicative f => (a -> f b) -> (b -> f c) -> a -> f c
kleisliA = undefined

joinA :: (Applicative f) => f (f a) -> f a
joinA = undefined

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA f x = undefined

liftA2 :: (Applicative f) => (a -> b -> r) -> f a -> f b -> f r
liftA2 f x y = undefined
