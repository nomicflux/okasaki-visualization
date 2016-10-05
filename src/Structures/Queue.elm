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

push : a -> Queue a -> Queue a
push val (Queue left right) = Queue (S.cons val left) right

dequeue : Queue a -> Maybe (Queue a)
dequeue (Queue left right) =
    case left of
        S.Cons _ rst -> Just (Queue rst right)
        S.Nil -> case (S.reverse right) of
                     S.Nil -> Nothing
                     S.Cons _ rst -> Just (Queue rst S.empty)

map : (a -> b) -> Queue a -> Queue b
map f (Queue left right) = Queue (S.map f left) (S.map f right)

toStack : Queue a -> S.Stack a
toStack (Queue left right) = S.append left (S.reverse right)

foldr : (a -> b -> b) -> b -> Queue a -> b
foldr f def queue = S.foldr f def (toStack queue)

foldl : (b -> a -> b) -> b -> Queue a -> b
foldl f def queue = S.foldl f def (toStack queue)

countLeft : Queue a -> Int
countLeft (Queue left _) = S.count left

countRight : Queue a -> Int
countRight (Queue _ right) = S.count right

count : Queue a -> Int
count queue = countLeft queue + countRight queue

left : Queue a -> S.Stack a
left (Queue left _) = left

right : Queue a -> S.Stack a
right (Queue _ right) = right
