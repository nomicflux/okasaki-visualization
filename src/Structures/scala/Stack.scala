package okasaki.stack;

// | *Stack head tail reverse cons
sealed abstract class Stack[+A]
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
  def cons[B >: A](x : B): Stack[B] = Cons(x, this)
  // .end
  // | *isEmpty
  def isEmpty: Boolean
  // .end
}

// | *Stack empty head tail reverse
case object Nil extends Stack[Nothing]
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
  // | *isEmpty
  def isEmpty = true
  // .end
  override def toString = "[]"
}

// | *Stack head tail reverse
case class Cons[A](head: A, tail: Stack[A]) extends Stack[A]
// .end
{
  // | *reverse
  def reverse = {
    def reverseHelp(curr: Stack[A], acc: Stack[A]): Stack[A] = curr match {
      case Nil => acc
      case Cons(h, t) => reverseHelp(t, Cons(h, acc))
    }
    reverseHelp(this, Nil)
  }
  // .end
  // | *isEmpty
  def isEmpty = false
  // .end

  override def toString = head.toString ++ " : " ++ tail.toString
}
