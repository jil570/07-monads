{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Monads where

import Control.Monad (guard)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Prelude hiding ((>>))

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

-- | zip two trees together
zipTree :: Tree a -> Tree b -> Tree (a, b)
zipTree = undefined

testZip0 :: Bool
testZip0 =
  zipTree
    (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
    (Branch (Leaf (0 :: Int)) (Branch (Leaf 1) (Leaf 2)))
    == Branch (Leaf ("a", 0)) (Branch (Leaf ("b", 1)) (Leaf ("c", 2)))

zipTree1 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree1 = undefined

testZip :: (Tree String -> Tree Int -> Maybe (Tree (String, Int))) -> Bool
testZip zt =
  zt
    (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
    (Branch (Leaf 0) (Branch (Leaf 1) (Leaf 2)))
    == Just (Branch (Leaf ("a", 0)) (Branch (Leaf ("b", 1)) (Leaf ("c", 2))))
    && Maybe.isNothing (zt (Branch (Leaf "a") (Leaf "b")) (Leaf 0))

--       SPOILER SPACE BELOW. Don't look until you finish zipTree1.

--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |
--        |

zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree2 (Leaf a) (Leaf b) = Just (Leaf (a, b))
zipTree2 (Branch l r) (Branch l' r') =
  case zipTree2 l l' of
    Nothing -> Nothing
    Just x -> case zipTree2 r r' of
      Nothing -> Nothing
      Just y -> Just (Branch x y)
zipTree2 _ _ = Nothing

retrn :: a -> Maybe a
retrn = Just

bind x f = case x of
  Nothing -> Nothing
  Just y -> f y

zipTree3 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree3 (Leaf a) (Leaf b) = retrn (Leaf (a, b))
zipTree3 (Branch l r) (Branch l' r') =
  zipTree3 l l'
    `bind` ( \x ->
               zipTree3 r r' `bind` \y -> retrn (Branch x y)
           )
zipTree3 _ _ = Nothing

zipTree4 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree4 (Leaf a) (Leaf b) = return (Leaf (a, b))
zipTree4 (Branch l r) (Branch l' r') =
  zipTree3 l l'
    >>= ( \x ->
            zipTree3 r r'
              >>= ( \y ->
                      return (Branch x y)
                  )
        )
zipTree4 _ _ = Nothing

zipTree5 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree5 (Leaf a) (Leaf b) = return (Leaf (a, b))
zipTree5 (Branch l r) (Branch l' r') = do
  undefined
zipTree5 _ _ = Nothing

main :: IO ()
main = do
  putStrLn "Hello. What is your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

(>>) :: Monad m => m a -> m b -> m b
m1 >> m2 = m1 >>= const m2

zipTree6 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree6 (Leaf a) (Leaf b) = pure (Leaf (a, b))
zipTree6 (Branch l r) (Branch l' r') =
  pure Branch <*> zipTree6 l l' <*> zipTree6 r r'
zipTree6 _ _ = Nothing

zipTree7 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree7 (Leaf a) (Leaf b) = pure (Leaf (a, b))
zipTree7 (Branch l r) (Branch l' r') =
  Branch <$> zipTree7 l l' <*> zipTree7 r r'
zipTree7 _ _ = Nothing

fmapMonad :: (Monad m) => (a -> b) -> m a -> m b
fmapMonad = undefined

pureMonad :: (Monad m) => a -> m a
pureMonad = undefined

zapMonad :: (Monad m) => m (a -> b) -> m a -> m b
zapMonad = undefined

pairs0 :: [a] -> [b] -> [(a, b)]
pairs0 xs ys = undefined

testPairs :: ([Int] -> [Int] -> [(Int, Int)]) -> Bool
testPairs ps =
  ps [1, 2, 3, 4] [5, 6, 7, 8]
    == [ (1, 5),
         (1, 6),
         (1, 7),
         (1, 8),
         (2, 5),
         (2, 6),
         (2, 7),
         (2, 8),
         (3, 5),
         (3, 6),
         (3, 7),
         (3, 8),
         (4, 5),
         (4, 6),
         (4, 7),
         (4, 8)
       ]

--      SPOILER SPACE BELOW

--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |
--       |

pairs1 :: [a] -> [b] -> [(a, b)]
pairs1 xs ys =
  concat
    ( map
        ( \x ->
            concat
              ( map
                  ( \y ->
                      [(x, y)]
                  )
                  ys
              )
        )
        xs
    )

pairs2 :: [a] -> [b] -> [(a, b)]
pairs2 xs ys = undefined

pairs3 :: [a] -> [b] -> [(a, b)]
pairs3 xs ys = undefined

testPairs2 :: Bool
testPairs2 = testPairs pairs2

testPairs3 :: Bool
testPairs3 = testPairs pairs3

pairs4 :: [Int] -> [Int] -> [(Int, Int)]
pairs4 xs ys = [(x, y) | x <- xs, y <- ys]

pairs5 :: [Int] -> [Int] -> [(Int, Int)]
pairs5 xs ys = [(x, y) | x <- xs, y <- ys, x /= y]

pairs5' :: [Int] -> [Int] -> [(Int, Int)]
pairs5' xs ys = do
  x <- xs
  y <- ys
  guard (x /= y) --- remember that no `<-` means `>>`, i.e. `>>=` ignoring the argument
  return (x, y)

data Color = Red | Green | Blue | Yellow | Orange | Violet deriving (Show, Enum, Eq)

stateColors :: [Color] -> [(Color, Color, Color, Color, Color)]
stateColors colors =
  [ (tennessee, mississippi, alabama, georgia, florida)
    | tennessee <- colors,
      mississippi <- colors,
      alabama <- colors,
      georgia <- colors,
      florida <- colors,
      tennessee /= mississippi, -- ensure neighboring states have different colors
      tennessee /= alabama,
      tennessee /= georgia,
      mississippi /= alabama,
      alabama /= georgia,
      florida /= alabama,
      florida /= georgia
  ]

colorsNeeded :: Maybe [Color]
colorsNeeded = List.find (not . null . stateColors) cs
  where
    cs :: [[Color]] -- i.e. [[Red], [Red,Green], [Red,Green,Blue], ...]
    cs = zipWith take [1 ..] (replicate 6 [Red ..])

map' :: (a -> b) -> [a] -> [b]
map' f xs = undefined

firstLess :: Ord a => [a] -> [a] -> [(a, a)]
firstLess = undefined

map1 :: (a -> b) -> [a] -> [b]
map1 = undefined

firstLess1 :: Ord a => [a] -> [a] -> [(a, a)]
firstLess1 xs ys = undefined

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = undefined

pairs6 xs ys = undefined
