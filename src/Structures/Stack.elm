module Structures.Stack exposing (..)

import Maybe exposing (Maybe)

type Stack a = Nil
             | Cons a (Stack a)

empty : Stack a
empty = Nil

isEmpty : Stack a -> Bool
isEmpty stack =
    case stack of
        Nil -> True
        Cons _ _ -> False

cons : a -> Stack a -> Stack a
cons = Cons

head : Stack a -> Maybe a
head stack =
    case stack of
        Nil -> Nothing
        Cons val _ -> Just val

tail : Stack a -> Maybe (Stack a)
tail stack =
    case stack of
        Nil -> Nothing
        Cons _ rst -> Just rst

map : (a -> b) -> Stack a -> Stack b
map f stack =
    case stack of
        Nil -> Nil
        Cons val rst -> Cons (f val) (map f rst)

enumerate : (Int -> a -> b) -> Stack a -> Stack b
enumerate f stack =
    let
        go s n =
            case s of
                Nil -> Nil
                Cons val rst -> Cons (f n val) (go rst (n + 1))
    in
       go stack 0

count : Stack a -> Int
count stack =
    case stack of
        Nil -> 0
        Cons _ rst -> 1 + count rst

foldr : (a -> b -> b) -> b -> Stack a -> b
foldr f def stack =
    case stack of
        Nil -> def
        Cons val rst -> f val (foldr f def rst)

foldl : (b -> a -> b) -> b -> Stack a -> b
foldl f def stack =
    case stack of
        Nil -> def
        Cons val rst -> f (foldl f def rst) val

reverse : Stack a -> Stack a
reverse stack =
    let
        go acc s =
            case s of
                Nil -> acc
                Cons val rst -> go (cons val acc) rst
    in
        go empty stack

toList : Stack a -> List a
toList stack = foldr (::) [] stack
