package Structures.Stack

// | *DataStructure Stack
abstract class Stack[+A]
// .end
{
  def head: A
  def tail: Stack[A]
  def cons(A): Stack[A]
  def reverse: Stack[A]
  // | *cons
  def cons(x : A) = Cons(x, this)
  // .end
}

// | *DataStructure Stack
case class Nil[Nothing] extends Stack
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

// | *DataStructure Stack
case class Cons[+A](head: A, tail: Stack[A]) extends Stack
// .end
{
  // | *reverse
  def reverse = {
    def reverseHelp(curr: Stack[A], acc: Stack[A]): Stack[A] = curr match {
      Nil => acc
      Cons(h, t) => reverseHelp(t, Cons(h, acc))
    }
    reverseHelp(this)
  }
  // .end
}
