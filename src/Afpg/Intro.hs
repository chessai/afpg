module Afpg.Intro
  (
  ) where

import Prelude hiding (Semigroup(..),Monoid(..),Maybe(..))

-- Laws:
--   Associativity: (x <> y) <> z = x <> (y <> z)
class Semigroup a where
  (<>) :: a -> a -> a

-- Laws:
--   Left  Identity: mempty <> a = a
--   Right Identity: a <> mempty = a
class Semigroup a => Monoid a where
  mempty :: a
  mconcat :: [a] -> a
  mconcat = foldr (<>) mempty

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

data Maybe a = Nothing | Just a

instance Semigroup a => Semigroup (Maybe a) where
  x       <> Nothing = x
  Nothing <> y       = y
  Just x  <> Just y  = Just (x <> y)

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance Semigroup Bool where
  (<>) = (&&)

instance Monoid Bool where
  mempty = undefined

instance Semigroup Integer where
  (<>) = (+)

instance Monoid Integer where
  mempty = 0
