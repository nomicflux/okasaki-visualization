module Stack where

-- | *DataStructure Stack
data Stack a = Nil
             | Cons a (Stack a)
-- .end

-- | *empty
empty :: Stack a
empty = Nil
-- .end

-- | *head
head :: Stack a -> Maybe a
head Nil = Nothing
head (Cons x _) = Just x
-- .end

-- | *tail
tail :: Stack a -> Maybe (Stack a)
tail Nil = Nothing
tail (Cons _ xs) = Just xs
-- .end

-- | *cons
cons :: a -> Stack a -> Stack a
cons val stack = Cons val stack
-- .end

-- | *reverse
reverse :: Stack a -> Stack a
reverse stack = go stack Nil
  where
    go :: Stack a -> Stack a -> Stack a
    go Nil acc = acc
    go (Cons x xs) acc = go xs (Cons x acc)
-- .end
