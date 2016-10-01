module Structure.Stack where

import Prelude ((-), pure, (<$>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)

data Stack a = Nil
             | Cons a (Stack a)

empty :: forall a. Stack a
empty = Nil

isEmpty :: forall a. Stack a -> Boolean
isEmpty Nil = false
isEmpty (Cons _ _) = true

cons :: forall a. a -> Stack a -> Stack a
cons val stack = Cons val stack

head :: forall a eff. Stack a -> Eff (err :: EXCEPTION | eff) a
head Nil = throw "Can't take head of empty list"
head (Cons val _) = pure val

tail :: forall a eff. Stack a -> Eff (err :: EXCEPTION | eff) (Stack a)
tail Nil = throw "Can't take tail of empty list"
tail (Cons _ stack) = pure stack

append :: forall a. Stack a -> Stack a -> Stack a
append Nil stack2 = stack2
append (Cons val stack1) stack2 = Cons val (append stack1 stack2)

update :: forall a eff. Stack a -> Int -> a -> Eff (err :: EXCEPTION | eff) (Stack a)
update Nil _ _ = throw "Out of bounds in update"
update (Cons _ stack) 0 newVal = pure (Cons newVal stack)
update (Cons val stack) n newVal = Cons val <$> update stack (n - 1) newVal
