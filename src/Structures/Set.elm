module Structures.Set exposing (..)

type Tree comparable = Leaf
                     | Tree (Tree comparable) comparable (Tree comparable)

empty : Tree comparable
empty = Leaf

isEmpty : Tree comparable -> Bool
isEmpty tree =
    case tree of
        Leaf -> True
        Tree _ _ _ -> False

member : comparable -> Tree comparable -> Bool
member val tree =
    case tree of
        Leaf -> False
        Tree l c r ->
            case compare val c of
                EQ -> True
                LT -> member val l
                GT -> member val r

insert : comparable -> Tree comparable -> Tree comparable
insert val tree =
    case tree of
        Leaf -> Tree Leaf val Leaf
        Tree l c r ->
            case compare val c of
                EQ -> tree
                LT -> Tree (insert val l) c r
                GT -> Tree l c (insert val r)

map : (comparable -> comparable) -> Tree comparable -> Tree comparable
map f tree =
    case tree of
        Leaf -> tree
        Tree l c r -> Tree (map f l) (f c) (map f r)
