module Structures.Purs.Leftist where

import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Ord (class Ord)
import Data.Semigroup (append)
import Data.Show (class Show, show)
-- import Data.Tuple (Tuple(..), fst, snd)
import Prelude (($), (+), (<>), (*), (-), max, (>=), (<=), (<), (==), (<<<))

import Structures.Purs.Stack as S

-- | *LeftistHeap
data Leftist a = Leaf
               | Node { left :: Leftist a
                      , value :: a
                      , right :: Leftist a
                      , rank :: Int
                      }
-- .end

-- | *empty
empty :: forall a. Leftist a
empty = Leaf
-- .end

-- | *merge insert deleteMin
rank :: forall a. Leftist a -> Int
rank Leaf = 0
rank (Node node) = node.rank
-- .end

-- | *merge insert deleteMin
makeNode :: forall a. a -> Leftist a -> Leftist a -> Leftist a
makeNode x a b =
  let
    rankA = rank a
    rankB = rank b
  in
   if rankA >= rankB
   then Node { rank : rankB + 1
             , value : x
             , left : a
             , right : b
             }
   else Node { rank : rankA + 1
             , value : x
             , left : b
             , right : a}
-- .end

-- | *merge insert deleteMin
merge :: forall a. Ord a => Leftist a -> Leftist a -> Leftist a
merge heap Leaf = heap
merge Leaf heap = heap
merge ha@(Node a) hb@(Node b) =
  if a.value <= b.value
  then makeNode a.value a.left (merge a.right hb)
  else makeNode b.value b.left (merge ha b.right)
-- .end

-- | *insert
singleton :: forall a. a -> Leftist a
singleton val = Node { value : val
                     , left : Leaf
                     , right : Leaf
                     , rank : 1
                     }
-- .end

-- | *insert
insert :: forall a. Ord a => Leftist a -> a -> Leftist a
insert node val = merge (singleton val) node
-- .end

-- | *findMin
findMin :: forall a. Leftist a -> Maybe a
findMin Leaf = Nothing
findMin (Node node) = Just node.value
-- .end

-- | *deleteMin
deleteMin :: forall a. Ord a => Leftist a -> Maybe (Leftist a)
deleteMin Leaf = Nothing
deleteMin (Node node) = Just $ merge node.left node.right
-- .end

toStack :: forall a. Leftist a -> S.Stack a
toStack Leaf = S.Nil
toStack (Node set) =
  let
    left = toStack set.left
    right = toStack set.right
  in
   append left (S.cons set.value right)

get :: forall a. Ord a => Leftist a -> a -> Maybe a
get heap val = S.toMaybe $
               filterMap (\n -> if n == val then Just n else Nothing) (toStack heap)

update :: forall a. Ord a => Leftist a -> a -> Leftist a
update heap val = map (\n -> if n == val then val else n) heap

depth :: forall a. Leftist a -> Int
depth Leaf = 0
depth (Node node) =
  let
    leftDepth = depth node.left
    rightDepth = depth node.right
  in
   1 + max leftDepth rightDepth

instance functorLeftist :: Functor Leftist where
  map _ Leaf = Leaf
  map f (Node node) = Node (node { left = map f node.left
                                 , value = f node.value
                                 , right = map f node.right
                                 })

instance foldableLeftist :: Foldable Leftist where
  foldr f def = foldr f def <<< toStack
  foldl f def = foldl f def <<< toStack

  foldMap _ Leaf = mempty
  foldMap f (Node node) =
    let
      left = foldMap f node.left
      right = foldMap f node.right
    in
     append (f node.value) (append left right)

instance showLeftist :: Show a => Show (Leftist a) where
  show Leaf = "."
  show (Node node) = show node.left <> " / " <> show node.value <> " \\ " <> show node.right
