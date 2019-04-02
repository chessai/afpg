{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Afpg.Examples where

import Data.Monoid
import Data.Semigroup
import Prelude hiding (sum,product,any,all,min,max,length)
import Data.Functor.Contravariant

import Control.Monad.Trans.Writer

import Data.Foldable (foldlM)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception

--foldMap :: Monoid m => (a -> m) -> [a] -> m
--foldMap f [] = mempty
--foldMap f (x:xs) = f x <> foldMap f xs

sum :: Num a => [a] -> a
sum = getSum . foldMap Sum

length :: [a] -> Int
length = getSum . foldMap (const (Sum 1))

product :: Num a => [a] -> a
product = getProduct . foldMap Product

any :: (a -> Bool) -> [a] -> Bool
any f = getAny . foldMap (Any . f)

all :: (a -> Bool) -> [a] -> Bool
all f = getAll . foldMap (All . f)

min :: (Ord a, Bounded a) => [a] -> a
min = getMin . foldMap Min

max :: (Ord a, Bounded a) => [a] -> a
max = getMax . foldMap Max

helloWorld :: String
helloWorld = foldMap id ["Hello, ", "World!"]

worldHello :: String
worldHello = getDual . foldMap Dual $ ["Hello, ", "World!"]

endoSum :: Num a => [a] -> a
endoSum = flip appEndo 0 . foldMap (Endo . (+))

isEven, isOdd, isDivisibleByThree :: Predicate Int
isEven = Predicate even
isOdd  = Predicate odd
isDivisibleByThree = Predicate (\x -> x `mod` 3 == 0)

-- | Log the steps it took to compute the factorial.
fact1 :: Int -> Writer String Int
fact1 0 = pure 1
fact1 n = do
  let n' = n - 1
  tell $ "We've taken one away from " <> show n <> "\n"
  m <- fact1 n'
  tell $ "We've called f " <> show m <> "\n"
  let r = n * m
  tell $ "We've multiplied " <> show n <> " and " <> show m <> "\n"
  pure r

ex1 = runWriter (fact1 10)

-- | Count how many multiplications and subtractions it
--   took to calculate a given factorial.
fact2 :: Int -> Writer (Sum Int) Int
fact2 0 = pure 1
fact2 n = do
  let n' = n - 1
  tell $ Sum 1
  m <- fact2 n'
  let r = n * m
  tell $ Sum 1
  pure r

ex2 = runWriter (fact2 10)

-- | Report if any of the intermediate results were '120'.
fact3 :: Int -> Writer Any Int
fact3 0 = pure 1
fact3 n = do
  let n' = n - 1
  m <- fact3 n'
  let r = n * m
  tell (Any (r == 120))
  pure r

ex3 = runWriter (fact3 10)

-- | Log backwards!
fact4 :: Int -> Writer (Dual String) Int
fact4 0 = pure 1
fact4 n = do
  let n' = n - 1
  tell $ Dual $ "We've taken one away from " ++ show n ++ "\n"
  m <- fact4 n'
  tell $ Dual $ "We've called f " ++ show m ++ "\n"
  let r = n * m
  tell $ Dual $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
  pure r

ex4 = runWriter (fact4 10)

tellFst a = tell $ (a,mempty)
tellSnd b = tell $ (mempty,b)

fact5 :: Int -> Writer (String, Sum Int) Int
fact5 0 = pure 1
fact5 n = do
  let n' = n - 1
  tellSnd (Sum 1)
  tellFst $ "We've taken one away from " ++ show n ++ "\n"
  m <- fact5 n'
  let r = n * m
  tellSnd (Sum 1)
  tellFst $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
  pure r

ex5 = runWriter (fact5 10)

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

tree = Node (Leaf 1) 7 (Node (Leaf 3) 4 (Leaf 10))

exTree1 = foldMap (Any . (==1)) tree
exTree2 = foldMap (All . (> 5)) tree

combineIO :: Monoid a => IO a -> IO a -> IO a
combineIO x y = do
  x' <- x
  y' <- y
  pure (x' <> y')

exIO1 = foldMap (print . Sum) [1..100]

{-
foldCommuteIO :: forall t m a. (Foldable t, Monoid m)
  => (a -> IO m)
  -> t a
  -> IO m
foldCommuteIO f xs = do
  var <- newEmptyMVar
  total <- foldlM
    (\ !n a -> forkIO_
         (try (f a) >>= putMVar var)
         *> pure (n + 1)
    ) 0 xs
  let go2 :: Int -> SomeException -> IO (Either SomeException m)
      go2 !n e = if n < total
        then takeMVar var *> go2 (n + 1) e
        else pure (Left e)
  let go :: Int -> m -> IO (Either SomeException m)
      go !n !m = if n < total
        then takeMVar var >>= \case
          Left r -> go2 (n + 1) r
          Right m' -> go (n + 1) (m <> m')
        else pure (Right m)
  x <- go 0 mempty
  case x of
    Left e -> error $ "Exception encountered:\n " <> show e
    Right m -> pure m

forkIO_ :: IO () -> IO ()
forkIO_ x = () <$ forkIO x
-}
