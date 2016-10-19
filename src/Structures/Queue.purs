module Structures.Queue where

import Data.Semigroup (append)
-- import Data.Monoid (mempty)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))

import Structures.Stack as S

-- | *DataStructure Queue
data Queue a = Queue { front :: S.Stack a
                     , back :: S.Stack a
                     }
-- .end

-- | *empty
empty :: forall a. Queue a
empty = Queue { front : S.empty
              , back : S.empty
              }
-- .end

-- | *top
top :: forall a. Queue a -> Maybe a
top (Queue queue) =
  case S.head (queue.front) of
    Just x -> Just x
    Nothing ->
      case S.head (S.reverse queue.back) of
        Nothing -> Nothing
        Just y -> Just y
-- .end

-- | *back
back :: forall a. Queue a -> Maybe a
back (Queue queue) =
  case S.head (queue.back) of
    Just y -> Just y
    Nothing ->
      case S.head (S.reverse queue.front) of
        Nothing -> Nothing
        Just x -> Just x
-- .end

-- | *pop
pop :: forall a. Queue a -> Maybe (Queue a)
pop (Queue queue) =
  case S.tail (queue.front) of
    Just xs -> Just (Queue (queue { front = xs }))
    Nothing ->
      case S.tail (S.reverse queue.back) of
        Nothing -> Nothing
        Just ys -> Just (Queue (queue { back = S.empty
                                      , front = ys}))
-- .end

-- | *eject
eject :: forall a. Queue a -> Maybe (Queue a)
eject (Queue queue) =
  case S.tail (queue.back) of
    Just xs -> Just (Queue (queue { back = xs }))
    Nothing ->
      case S.tail (S.reverse queue.front) of
        Nothing -> Nothing
        Just ys -> Just (Queue (queue { front = S.empty
                                      , back = ys}))
-- .end

-- | *push
push :: forall a. a -> Queue a -> Queue a
push val (Queue queue) = Queue (queue { front = S.cons val queue.front })
-- .end

-- | *inject
inject :: forall a. a -> Queue a -> Queue a
inject val (Queue queue) = Queue (queue { back = S.cons val queue.back })
-- end

topHead :: forall a. Queue a -> Maybe a
topHead (Queue queue) = S.head queue.front

backHead :: forall a. Queue a -> Maybe a
backHead (Queue queue) = S.head queue.back

toStack :: forall a. Queue a -> S.Stack a
toStack (Queue queue) = append queue.front (S.reverse queue.back)

biCount :: forall a. Queue a -> Tuple Int Int
biCount (Queue queue) = Tuple (S.count queue.front) (S.count queue.back)

instance functorQueue :: Functor Queue where
  map f (Queue queue) = Queue (queue { front = map f queue.front, back = map f queue.back })

instance foldableQueue :: Foldable Queue where
  foldr f def queue = foldr f def (toStack queue)
  foldl f def queue = foldl f def (toStack queue)
  foldMap f queue = foldMap f (toStack queue)

instance showQueue :: (Show a) => Show (Queue a) where
  show (Queue queue) = (show queue.front) <> " // " <> (show queue.back)
