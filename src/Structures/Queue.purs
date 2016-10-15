module Structures.Queue where

import Data.Semigroup (append)
import Data.Monoid (mempty)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..))

import Structures.Stack as S

data Queue a = Queue { front :: S.Stack a
                     , back :: S.Stack a
                     }

empty :: forall a. Queue a
empty = Queue { front : S.empty
              , back : S.empty
              }

top :: forall a. Queue a -> Maybe a
top (Queue queue) =
  case S.head (queue.front) of
    Just x -> Just x
    Nothing ->
      case S.head (S.reverse queue.back) of
        Nothing -> Nothing
        Just y -> Just y

back :: forall a. Queue a -> Maybe a
back (Queue queue) =
  case S.head (queue.back) of
    Just y -> Just y
    Nothing ->
      case S.head (S.reverse queue.front) of
        Nothing -> Nothing
        Just x -> Just x

pop :: forall a. Queue a -> Maybe (Queue a)
pop (Queue queue) =
  case S.tail (queue.front) of
    Just xs -> Just (Queue (queue { front = xs }))
    Nothing ->
      case S.tail (S.reverse queue.back) of
        Nothing -> Nothing
        Just ys -> Just (Queue (queue { front = ys}))

eject :: forall a. Queue a -> Maybe (Queue a)
eject (Queue queue) =
  case S.tail (queue.back) of
    Just xs -> Just (Queue (queue { back = xs }))
    Nothing ->
      case S.tail (S.reverse queue.front) of
        Nothing -> Nothing
        Just ys -> Just (Queue (queue { back = ys}))

push :: forall a. Queue a -> a -> Queue a
push (Queue queue) val = Queue (queue { front = S.cons val queue.front })

inject :: forall a. Queue a -> a -> Queue a
inject (Queue queue) val = Queue (queue { back = S.cons val queue.back })

toStack :: forall a. Queue a -> S.Stack a
toStack (Queue queue) = append queue.front (S.reverse queue.back)

instance functorQueue :: Functor Queue where
  map f (Queue queue) = Queue (queue { front = map f queue.front, back = map f queue.back })

instance foldableQueue :: Foldable Queue where
  foldr f def queue = foldr f def (toStack queue)
  foldl f def queue = foldl f def (toStack queue)
  foldMap f queue = foldMap f (toStack queue)
