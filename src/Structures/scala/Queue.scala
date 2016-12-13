package okasaki;

// | *Queue empty
object Queue
// .end
{
  // | *empty
  def empty = new Queue(Nil, Nil)
  // .end
}

// | *Queue isEmpty rotate top back pop push inject eject
sealed class Queue[+A](front: Stack[A], back: Stack[A])
// .end
{
  // | *isEmpty
  def isEmpty: Boolean =
    front.isEmpty && back.isEmpty
  // .end

  // | *rotate
  def rotate: Queue[A] =
    new Queue(back.reverse, front.reverse)
  // .end

  // | *top
  def top: A = front match {
    case Cons(h, _) => h
    case Nil => back.reverse match {
      case Cons(h, _) => h
      case Nil => throw new NoSuchElementException("Empty top")
    }
  }
  // .end

  // | *back
  def bottom: A = back match {
    case Cons(h, _) => h
    case Nil => front.reverse match {
      case Cons(h, _) => h
      case Nil => throw new NoSuchElementException("Empty back")
    }
  }
  // .end

  // | *pop
  def pop: Queue[A] = front match {
    case Cons(_, t) => new Queue(t, back)
    case Nil => back.reverse match {
      case Cons(_, t) => new Queue(t, Nil)
      case Nil => throw new NoSuchElementException("Empty pop")
    }
  }
  // .end

  // | *eject
  def eject: Queue[A] = back match {
    case Cons(_, t) => new Queue(front, t)
    case Nil => front.reverse match {
      case Cons(_, t) => new Queue(Nil, t)
      case Nil => throw new NoSuchElementException("Empty eject")
    }
  }
  // .end

  // | *push
  def push[B >: A](elem: B): Queue[B] =
    new Queue(front.cons(elem), back)
  // .end

  // | *inject
  def inject[B >: A](elem: B): Queue[B] =
    new Queue(front, back.cons(elem))
  // .end

  def shift[B >: A](elem: B): Queue[B] = inject(elem)
  def unshift: Queue[A] = pop

  override def toString = front.toString ++ " // " ++ back.toString
}
