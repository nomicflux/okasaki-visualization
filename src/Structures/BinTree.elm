module Structures.BinTree exposing (..)

type Tree a = Leaf
            | Tree (Tree a) a (Tree a)

empty : Tree a
empty = Leaf

isEmpty : Tree a -> Bool
isEmpty tree =
    case tree of
        Leaf -> True
        Tree _ _ _ -> False
