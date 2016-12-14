package okasaki.stack;

// | *Stack
sealed trait Stack[+A]
// .end
{
  // | *head
  // Nil case
  def head: A = throw new NoSuchElementException("Nil.head")
  // .end

  // | *tail
  // Nil case
  def tail: Stack[A] =  throw new NoSuchElementException("Nil.tail")
  // .end

  // | *reverse
  def reverse: Stack[A] = {
    def reverseHelp(curr: Stack[A], acc: Stack[A]): Stack[A] = curr match {
      case Nil => acc
      case Cons(h, t) => reverseHelp(t, Cons(h, acc))
    }
    reverseHelp(this, Nil)
  }
  // .end

  // | *cons
  def cons[B >: A](x : B): Stack[B] = Cons(x, this)
  // .end

  // | *isEmpty
  def isEmpty: Boolean = this match {
    case Nil => false
    case Cons(_, _) => true
  }
  // .end

  override def toString = this match {
    case Nil => "[]"
    case Cons(head,tail) => head.toString ++ " : " ++ tail.toString
  }
}

// | *Stack empty
case object Nil extends Stack[Nothing]
  // .end
// | *Stack head tail
case class Cons[+A](override val head: A,
                    override val tail: Stack[A]) extends Stack[A]
// .end
