module Queue

import Data.Vect

%access export

-- | *Queue
data Queue : Type -> Nat -> Nat -> Nat -> Type where
     mkQueue : (front : Vect n ty)
             -> (back : Vect m ty)
             -> Queue ty n m (n + m)

%name Queue queue
-- .end

-- | *top
rotate : Queue ty Z k k -> Queue ty k Z k
rotate {k} (mkQueue front back) =
  replace {P=Queue ty k Z} (plusZeroRightNeutral k) $ mkQueue (reverse back) front
-- .end

-- | *back
rotateBack' : Queue ty k Z (plus k Z) -> Queue ty Z k k
rotateBack' {k} (mkQueue front back) =
  mkQueue back (reverse front)

rotateBack : Queue ty k Z k -> Queue ty Z k k
rotateBack {k} queue =
  let queue' = replace {P=Queue ty k Z} (sym $ plusZeroRightNeutral k) queue
  in rotateBack' queue'
-- .end

-- | *top
top : Queue ty n m (S k) -> ty
top {n = S j} {m} {k = j + m} (mkQueue front back) =
    head front
top {n = Z} {m = S j} {k = j} (mkQueue front back) =
    head $ reverse back
-- .end

-- | *back
bottom : Queue ty n m (S k) -> ty
bottom {m = Z} queue = ?bottomHole $ rotateBack queue
-- .end
