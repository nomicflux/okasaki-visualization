module Queue

import Stack as Stack

%access export
%default total

-- | *Queue
data Queue : Type -> Nat -> Nat -> Nat -> Type where
     MkQueue : (front : Stack.Stack n ty)
             -> (back : Stack.Stack m ty)
             -> Queue ty n m (n + m)

%name Queue queue
-- .end

-- | *empty
empty : Queue ty Z Z Z
empty = MkQueue Stack.empty Stack.empty
-- .end

-- | *rotate
rotate : Queue ty m n k -> Queue ty n m k
rotate {m} {n} (MkQueue front back) =
       rewrite (plusCommutative m n) in MkQueue back front
-- .end

-- | *top
top : Queue ty n m (S k) -> ty
top {n = S n'} {m = _} (MkQueue front _) =
    Stack.head front
top {n = Z} {m = S m'} (MkQueue _ back) =
    Stack.head $ Stack.reverse back
top {n = Z} {m = Z} (MkQueue _ _) impossible
-- .end

-- | *back
bottom : Queue ty n m (S k) -> ty
bottom {m = S m'} {n = S n'} (MkQueue _ back) =
       Stack.head back
bottom {m = S m'} {n = Z} (MkQueue _ back) =
       Stack.head back
bottom {m = Z} {n = S j} (MkQueue front _) =
       Stack.head $ Stack.reverse front
bottom {m = Z} {n = Z} (MkQueue _ _) impossible
-- .end

-- | *inject
inject : (a : ty) -> Queue ty n m k -> Queue ty n (S m) (S k)
inject {n} {m} newVal (MkQueue front back) =
       rewrite (plusSuccRightSucc n m) in MkQueue front (Stack.cons newVal back)
-- .end

-- | *push
push : (a : ty) -> Queue ty n m k -> Queue ty (S n) m (S k)
push newVal (MkQueue front back) = MkQueue (Stack.cons newVal front) back
-- .end
