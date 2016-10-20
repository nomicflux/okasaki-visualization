module Queue exposing (..)

import Maybe exposing (Maybe(..))
import Stack as Stack

-- | *Queue
type Queue a = Queue { front : Stack.Stack a
                     , back : Stack.Stack a
                     }
-- .end

-- | *empty
empty : Queue a
empty = Queue { front = Stack.empty
              , back = Stack.empty
              }
-- .end

-- | *top
top : Queue a -> Maybe a
top (Queue queue) =
    case Stack.head queue.front of
        Just x -> Just x
        Nothing ->
            case queue.back |> Stack.reverse |> Stack.head of
                Nothing -> Nothing
                Just y -> Just y
-- .end

-- | *back
back : Queue a -> Maybe a
back (Queue queue) =
    case Stack.head queue.back of
        Just y -> Just y
        Nothing ->
            case queue.front |> Stack.reverse |> Stack.head of
                Nothing -> Nothing
                Just x -> Just x
-- .end

-- | *pop
pop : Queue a -> Maybe (Queue a)
pop (Queue queue) =
    case Stack.tail queue.front of
        Just xs -> Queue { queue | front = xs } |> Just
        Nothing ->
            case queue.back |> Stack.reverse |> Stack.tail of
                Nothing -> Nothing
                Just ys -> Queue { front = ys, back = Stack.Nil } |> Just
-- .end

-- | *eject
eject : Queue a -> Maybe (Queue a)
eject (Queue queue) =
    case Stack.tail queue.back of
        Just ys -> Queue { queue | back = ys } |> Just
        Nothing ->
            case queue.front |> Stack.reverse |> Stack.tail of
                Nothing -> Nothing
                Just xs -> Queue { back = xs, front = Stack.Nil } |> Just
-- .end

-- | *inject
inject : a -> Queue a -> Queue a
inject val (Queue queue) =
    Queue { queue | back = Stack.cons val queue.back }
-- .end

-- | *push
push : a -> Queue a -> Queue a
push val (Queue queue) =
    Queue { queue | front = Stack.cons val queue.front }
-- .end
