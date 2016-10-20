module Structures.Stack exposing (..)

import Maybe exposing (Maybe(..))

-- | *DataStructure Stack
type Stack a = Nil
             | Cons a (Stack a)
-- .end

-- | *empty
empty : Stack a
empty = Nil
-- .end

-- | *head
head : Stack a -> Maybe a
head stack =
    case stack of
        Nil -> Nothing
        Cons x _ -> Just x
-- .end

-- | *tail
tail : Stack a -> Maybe (Stack a)
tail stack =
    case stack of
        Nil -> Nothing
        Cons _ xs -> Just xs
-- .end

-- | *cons
cons : a -> Stack a -> Stack a
cons val stack = Cons val stack
-- .end

-- | *reverse
reverse : Stack a -> Stack a
reverse stack = go stack Nil
    where
        go Nil acc = acc
        go (Cons x xs) acc = go xs (Cons x acc)
-- .end

map : (a -> b) -> Stack a -> Stack b
map f stack =
    case stack of
        Nil -> Nil
        Cons x xs -> Cons (f x) (map f xs)
