package okasaki.stack;

// | *Stack head tail reverse cons
sealed abstract class Stack[+A]
// .end
{
  // | *head
  // Class
  def head: A
  // .end
  // | *tail
  // Class
  def tail: Stack[A]
  // .end
  // | *reverse
  // Class
  def reverse: Stack[A]
  // .end
  // | *cons
  // Class
  def cons[B >: A](x : B): Stack[B] = Cons(x, this)
  // .end
  // | *isEmpty
  // Class
  def isEmpty: Boolean
  // .end
}

// | *Stack empty head tail reverse
case object Nil extends Stack[Nothing]
// .end
{
  // | *head
  // Nil
  def head = throw new NoSuchElementException("Nil.head")
  // .end
  // | *tail
  // Nil
  def tail = throw new NoSuchElementException("Nil.tail")
  // .end
  // | *reverse
  // Nil
  def reverse = this
  // .end
  // | *isEmpty
  // Nil
  def isEmpty = true
  // .end
  override def toString = "[]"
}

// | *Stack head tail reverse
case class Cons[A](head: A, tail: Stack[A]) extends Stack[A]
// .end
{
  // | *reverse
  // Cons
  def reverse = {
    def reverseHelp(curr: Stack[A], acc: Stack[A]): Stack[A] = curr match {
      case Nil => acc
      case Cons(h, t) => reverseHelp(t, Cons(h, acc))
    }
    reverseHelp(this, Nil)
  }
  // .end
  // | *isEmpty
  // Cons
  def isEmpty = false
  // .end

  override def toString = head.toString ++ " : " ++ tail.toString
}
