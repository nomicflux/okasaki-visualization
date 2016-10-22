module Structures.Purs.Set where

import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Ord (class Ord, compare, Ordering(..))
import Data.Semigroup (append)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (($), (+), (<>), (*), (-), max)

import Structures.Purs.Stack as S

-- | *Set
data Set a = Leaf
           | Node { left :: Set a
                  , value :: a
                  , right :: Set a
                  }
-- .end

-- | *empty
empty :: forall a. Set a
empty = Leaf
-- .end

-- | *insert
insert :: forall a. Ord a => Set a -> a -> Set a
insert Leaf val = Node { left : Leaf, value : val, right : Leaf }
insert s@(Node set) val =
  case compare val set.value of
    EQ -> s
    LT -> Node (set { left = insert set.left val })
    GT -> Node (set { right = insert set.right val })
-- .end

insertWithParent :: forall a. Ord a => Set a -> a -> Tuple (Set a) (Maybe a)
insertWithParent set val = go set val Nothing
  where
    go :: Set a -> a -> (Maybe a) -> Tuple (Set a) (Maybe a)
    go Leaf v mparent = Tuple (Node { left : Leaf, value : v, right : Leaf}) mparent
    go (Node s) v mparent =
      case compare v s.value of
        EQ -> Tuple (Node s) mparent
        LT ->
          let
            left = go s.left v (Just s.value)
          in
           Tuple (Node (s { left = fst left })) (snd left)
        GT ->
          let
            right = go s.right v (Just s.value)
          in
           Tuple (Node (s { right = fst right })) (snd right)

-- | *member
member :: forall a. Ord a => Set a -> a -> Boolean
member Leaf _ = false
member (Node set) val =
  case compare val set.value of
    EQ -> true
    LT -> member set.left val
    GT -> member set.right val
-- .end

get :: forall a. Ord a => Set a -> a -> Maybe a
get Leaf _ = Nothing
get (Node set) val =
  case compare val set.value of
    EQ -> Just set.value
    LT -> get set.left val
    GT -> get set.right val

update :: forall a. Ord a => Set a -> a -> Set a
update Leaf _ = Leaf
update (Node set) val =
  case compare val set.value of
    EQ -> Node $ set { value = val }
    LT -> Node $ set { left = update set.left val }
    GT -> Node $ set { right = update set.right val }

toStack :: forall a. Set a -> S.Stack a
toStack Leaf = S.Nil
toStack (Node set) =
  let
    left = toStack set.left
    right = toStack set.right
  in
   append left (S.cons set.value right)

depth :: forall a. Ord a => Set a -> Int
depth Leaf = 0
depth (Node set) =
  let
    leftDepth = depth set.left
    rightDepth = depth set.right
    maxDepth = max leftDepth rightDepth
  in
   maxDepth + 1

width :: forall a. Ord a => Set a -> Int
width Leaf = 0
width (Node set) =
  let
    leftWidth = width set.left
    rightWidth = width set.right
  in
   1 + leftWidth + rightWidth

fan :: forall a. Ord a => Set a -> Int
fan Leaf = 0
fan (Node set) =
  let
    leftFan = width set.left
    rightFan = width set.right
  in
   1 + 2 * max leftFan rightFan

leftWidth :: forall a. Ord a => Set a -> Int
leftWidth Leaf = 0
leftWidth (Node node) =
  let
    left = leftWidth node.left + 1
    right = leftWidth node.right - 1
  in
   max left right

rightWidth :: forall a. Ord a => Set a -> Int
rightWidth Leaf = 0
rightWidth (Node node) =
  let
    left = rightWidth node.left - 1
    right = rightWidth node.right + 1
  in
   max left right

leftMaxWidth :: forall a. Ord a => Set a -> Int
leftMaxWidth Leaf = 0
leftMaxWidth (Node node) = maxWidth node.left

rightMaxWidth :: forall a. Ord a => Set a -> Int
rightMaxWidth Leaf = 0
rightMaxWidth (Node node) = maxWidth node.right

maxWidth :: forall a. Ord a => Set a -> Int
maxWidth node = 1 + 2 * max (leftWidth node) (rightWidth node)

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

instance showSet :: Show a => Show (Set a) where
  show Leaf = "."
  show (Node set) = show set.left <> " / " <> show set.value <> " \\ " <> show set.right
