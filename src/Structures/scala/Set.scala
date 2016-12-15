package okasaki.set;

// | *Set insert member
sealed abstract class Set[+A <% Ordered[A]]
// .end
{
  // | *insert
  def insert[B >: A <% Ordered[B]](newVal: B): Set[B] = this match {
    case Leaf =>
      new Node(Leaf, newVal, Leaf)
    case Node(l, v, r) =>
      if(newVal < v)
        new Node(l.insert(newVal), v, r)
      else if(newVal > v)
        new Node(l, v, r.insert(newVal))
      else
        this
  }
  // .end
  // | *member
  def member[B >: A <% Ordered[B]](valToCheck: B): Boolean = this match {
    case Leaf => false
    case Node(l, v, r) =>
      if(valToCheck < v)
        l.member(valToCheck)
      else if(valToCheck > v)
        r.member(valToCheck)
      else
        true
  }
  // .end
}

// | *Set
case object Leaf extends Set[Nothing]
// .end

// | *Set
case class Node[+A <% Ordered[A]](left: Set[A],
                                  value: A,
                                  right: Set[A])
    extends Set[A]
// .end

// | *Set
object Set
// .end
{
  // | *empty
  def empty[A <% Ordered[A]]: Set[A] = Leaf
  // .end

  def apply[A <% Ordered[A]](as: A*): Set[A] =
    as.foldLeft(Leaf: Set[A])(_ insert _)
}
