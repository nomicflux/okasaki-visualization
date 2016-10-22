// | *Stack
abstract class Stack[A]
// .end
{
  // | *head
  def head: A
  // .end
  // | *tail
  def tail: Stack[A]
  // .end
  // | *reverse
  def reverse: Stack[A]
  // .end
  // | *cons
  def cons(x : A) = Cons(x, this)
  // .end
}

// | *Stack empty head tail reverse
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

// | *Stack head tail reverse
case class Cons[A](head: A, tail: Stack[A]) extends Stack[A]
// .end
{
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
