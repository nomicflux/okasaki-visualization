module Structures.Stack where

import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup (class Semigroup, append)
import Prelude ((+))

data Stack a = Nil
             | Cons a (Stack a)

empty :: forall a. Stack a
empty = Nil

head :: forall a. Stack a -> Maybe a
head Nil = Nothing
head (Cons x _) = Just x

tail :: forall a. Stack a -> Maybe (Stack a)
tail Nil = Nothing
tail (Cons _ xs) = Just xs

cons :: forall a. a -> Stack a -> Stack a
cons val stack = Cons val stack

reverse :: forall a. Stack a -> Stack a
reverse stack = go stack Nil
  where
    go :: Stack a -> Stack a -> Stack a
    go Nil acc = acc
    go (Cons x xs) acc = go xs (Cons x acc)

instance functorStack :: Functor Stack where
  map _ Nil = Nil
  map f (Cons x xs) = Cons (f x) (map f xs)

instance sgroupStack :: Semigroup (Stack a) where
  append Nil stack = stack
  append (Cons x xs) stack = Cons x (append xs stack)

instance monoidStack :: Monoid (Stack a) where
  mempty = Nil

instance foldableStack :: Foldable Stack where
  foldr _ def Nil = def
  foldr f def (Cons x xs) = f x (foldr f def xs)

  foldl _ def Nil = def
  foldl f def (Cons x xs) = f (foldl f def xs) x

  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = append (f x) (foldMap f xs)

count :: forall a. Stack a -> Int
count = foldr (\ _ acc -> acc + 1) 0
