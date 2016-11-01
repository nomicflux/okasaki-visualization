{-# LANGUAGE RecordWildCards #-}
module Queue where

import qualified Stack as Stack

-- | *Queue
data Queue a = Queue { queueFront :: Stack.Stack a
                     , queueBack :: Stack.Stack a
                     }
-- .end

-- | *empty
empty :: Queue a
empty = Queue { queueFront = Stack.empty
              , queueBack = Stack.empty
              }
-- .end

-- | *rotate
rotate :: Queue a -> Queue a
rotate Queue{..} =
  Queue { queueFront = Stack.reverse queueBack
        , queueBack = Stack.reverse queueFront
        }
-- .end

-- | *top
top :: Queue a -> Maybe a
top Queue{..} =
  case (Stack.head queueFront, Stack.head $ Stack.reverse queueBack) of
    (Nothing, Nothing) -> Nothing
    (Just x, _) -> Just x
    (Nothing, Just y) -> Just y
-- .end

-- | *back
back :: Queue a -> Maybe a
back Queue{..} =
  case (Stack.head queueBack, Stack.head $ Stack.reverse queueFront) of
    (Nothing, Nothing) -> Nothing
    (Just y, _) -> Just y
    (Nothing, Just x) -> Just x
-- .end

-- | *pop
pop :: Queue a -> Maybe (Queue a)
pop Queue{..} =
  case (Stack.tail queueFront, Stack.tail $ Stack.reverse queueBack) of
    (Nothing, Nothing) -> Nothing
    (Just xs, _) -> Just $ Queue {queueFront = xs, queueBack = queueBack}
    (Nothing, Just ys) -> Just $ Queue {queueFront = ys, queueBack = Stack.empty}
-- .end

-- | *eject
eject :: Queue a -> Maybe (Queue a)
eject Queue{..} =
  case (Stack.tail queueBack, Stack.tail $ Stack.reverse queueFront) of
    (Nothing, Nothing) -> Nothing
    (Just ys, _) -> Just $ Queue {queueBack = ys, queueFront = queueFront}
    (Nothing, Just xs) -> Just $ Queue {queueBack = xs, queueFront = Stack.empty}
-- .end

-- | *push
push :: a -> Queue a -> Queue a
push val Queue{..} =
  Queue { queueFront = Stack.cons val queueFront
        , queueBack = queueBack
        }
-- .end

-- | *inject
inject :: a -> Queue a -> Queue a
inject val Queue{..} =
  Queue { queueBack = Stack.cons val queueBack
        , queueFront = queueFront
        }
-- .end
