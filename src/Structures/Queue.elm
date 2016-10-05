module Structures.Queue exposing (..)

import Maybe exposing (Maybe)

import Structures.Stack as S

type Queue a = Queue (S.Stack a) (S.Stack a)

empty : Queue a
empty = Queue (S.empty) (S.empty)

isEmpty : Queue a -> Bool
isEmpty (Queue left right) = (S.isEmpty left) && (S.isEmpty right)

enqueue : a -> Queue a -> Queue a
enqueue val (Queue left right) = Queue left (S.cons val right)

top : Queue a -> Maybe a
top (Queue left right) =
    case left of
        S.Cons val _ -> Just val
        S.Nil -> case (S.reverse right) of
                     S.Nil -> Nothing
                     S.Cons val _ -> Just val

dequeue : Queue a -> Maybe (Queue a)
dequeue (Queue left right) =
    case left of
        S.Cons _ rst -> Just (Queue rst right)
        S.Nil -> case (S.reverse right) of
                     S.Nil -> Nothing
                     S.Cons _ rst -> Just (Queue rst S.empty)

map : (a -> b) -> Queue a -> Queue b
map f (Queue left right) = Queue (S.map f left) (S.map f right)

foldr : (a -> b -> a) -> b -> Queue a -> b
foldr f def (Queue left right) = S.foldr f def (S.append left (S.reverse right))
