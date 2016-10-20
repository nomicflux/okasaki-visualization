// | *Stack
abstract class Stack[A]
// .end
{
  def head: A
  def tail: Stack[A]
  def reverse: Stack[A]
  // | *cons
  def cons(x : A) = Cons(x, this)
  // .end
}

// | *Stack
case class Nil[Nothing]() extends Stack[Nothing]
// .end
{
  // | *head
  def head = throw new NoSuchElementException("Nil.head")
  // .end
  // | *tail
  def tail = throw new NoSuchElementException("Nil.tail")
  // .end
  // | *reverse
  def reverse = this
  // .end
}

// | *Stack
case class Cons[A](head: A, tail: Stack[A]) extends Stack[A]
// .end
{
  // | *head
  // case class Cons[A](head: A, tail: Stack[A]) extends Stack[A]
  // .end
  // | *tail
  // case class Cons[A](head: A, tail: Stack[A]) extends Stack[A]
  // .end
  // | *reverse
  def reverse = {
    def reverseHelp(curr: Stack[A], acc: Stack[A]): Stack[A] = curr match {
      case Nil() => acc
      case Cons(h, t) => reverseHelp(t, Cons(h, acc))
    }
    reverseHelp(this, Nil())
  }
  // .end
}
