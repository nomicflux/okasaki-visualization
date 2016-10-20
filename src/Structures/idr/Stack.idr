module Stack

%access export
%default total

-- | *DataStructure Stack
data Stack : Nat -> Type -> Type where
     Nil : Stack Z a
     Cons : (x : a) -> (xs : Stack k a) -> Stack (S k) a
-- .end

-- | *empty
empty : Stack Z a
empty = Nil
-- .end

-- | *head
head : Stack (S k) a -> a
head (Cons x _xs) = x
-- .end

-- | *tail
tail : Stack (S k) a -> Stack k a
tail (Cons _x xs) = xs
-- .end

-- | *cons
cons : a -> Stack k a -> Stack (S k) a
cons x [] = Cons x []
cons x (Cons y xs) = Cons x (Cons y xs)
-- .end

-- | *reverse
reverse : Stack n a -> Stack n a
reverse {n} stack = go empty stack
  where
    go : (acc : Stack k a) -> (curr : Stack j a) -> Stack (k + j) a
    go {k} acc [] = rewrite plusZeroRightNeutral k in acc
    go {k} {j = S j'} acc (Cons x xs) =
       rewrite sym $ plusSuccRightSucc k j' in go (cons x acc) xs
-- .end
