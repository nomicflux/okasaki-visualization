module Structures.Purs.Stack where

import Data.Either (Either(..))
import Data.Filterable (class Filterable, filterMap, filter, partitionMap, partition)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty, (<>))
import Data.Semigroup (class Semigroup, append)
import Data.Show (class Show, show)
import Prelude ((+))

-- | *Stack
data Stack a = Nil
             | Cons a (Stack a)
-- .end

-- | *empty
empty :: forall a. Stack a
empty = Nil
-- .end

-- | *isEmpty
isEmpty :: forall a. Stack a -> Boolean
isEmpty Nil = true
isEmpty (Cons _ _) = false
-- .end

-- | *head
head :: forall a. Stack a -> Maybe a
head Nil = Nothing
head (Cons x _) = Just x
-- .end

-- | *tail
tail :: forall a. Stack a -> Maybe (Stack a)
tail Nil = Nothing
tail (Cons _ xs) = Just xs
-- .end

-- | *cons
cons :: forall a. a -> Stack a -> Stack a
cons val stack = Cons val stack
-- .end

-- | *reverse
reverse :: forall a. Stack a -> Stack a
reverse stack = go stack Nil
  where
    go :: Stack a -> Stack a -> Stack a
    go Nil acc = acc
    go (Cons x xs) acc = go xs (Cons x acc)
-- .end

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

instance showStack :: (Show a) => Show (Stack a) where
  show Nil = "[]"
  show (Cons x xs) = show x <> " : " <> show xs

toMaybe :: forall a. Stack a -> Maybe a
toMaybe Nil = Nothing
toMaybe (Cons x _) = Just x

instance filterableStack :: Filterable Stack where
  filter p Nil = Nil
  filter p (Cons x xs) =
    if p x
    then Cons x (filter p xs)
    else filter p xs

  filterMap f Nil = Nil
  filterMap f (Cons x xs) =
    case f x of
      Nothing -> filterMap f xs
      Just y -> Cons y (filterMap f xs)

  partition p Nil = { no : Nil, yes : Nil }
  partition p (Cons x xs) =
    let
      rest = partition p xs
    in
     if (p x)
     then { no : rest.no
          , yes : Cons x rest.yes
          }
     else { no : Cons x rest.no
          , yes : rest.yes
          }

  partitionMap p Nil = { left : Nil, right : Nil}
  partitionMap p (Cons x xs) =
    let
      rest = partitionMap p xs
    in
     case p x of
       Left l -> { left : Cons l rest.left
                 , right : rest.right
                 }
       Right r -> { left : rest.left
                  , right : Cons r rest.right
                  }
