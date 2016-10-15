module Structures.Set where

import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Functor (class Functor, map)
import Data.Monoid (mempty)
import Data.Ord (class Ord, compare, Ordering(..))
import Data.Semigroup (append)

import Structures.Stack as S

data Set a = Leaf
           | Node { left :: Set a
                  , value :: a
                  , right :: Set a
                  }

empty :: forall a. Set a
empty = Leaf

insert :: forall a. Ord a => Set a -> a -> Set a
insert Leaf val = Node { left : Leaf, value : val, right : Leaf }
insert s@(Node set) val =
  case compare val set.value of
    EQ -> s
    LT -> Node (set { left = insert set.left val })
    GT -> Node (set { right = insert set.right val })

member :: forall a. Ord a => Set a -> a -> Boolean
member Leaf _ = false
member (Node set) val =
  case compare val set.value of
    EQ -> true
    LT -> member set.left val
    GT -> member set.right val

toStack :: forall a. Set a -> S.Stack a
toStack Leaf = S.Nil
toStack (Node set) =
  let
    left = toStack set.left
    right = toStack set.right
  in
   append left (S.cons set.value right)


instance functorSet :: Functor Set where
  map _ Leaf = Leaf
  map f (Node set) = Node (set { left = map f set.left
                               , value = f set.value
                               , right = map f set.right
                               })

instance foldableSet :: Foldable Set where
  foldr f def set = foldr f def (toStack set)
  foldl f def set = foldl f def (toStack set)

  foldMap _ Leaf = mempty
  foldMap f (Node set) =
    let
      left = foldMap f set.left
      right = foldMap f set.right
    in
     append (f set.value) (append left right)
