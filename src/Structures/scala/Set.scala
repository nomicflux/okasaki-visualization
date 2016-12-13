package okasaki.set;

// | *Set insert member
sealed abstract class Set[+A <% Ordered[A]]
// .end
{
  // | *insert
  // Class
  def insert[B >: A <% Ordered[B]](value: B): Set[B]
  // .end
  // | *member
  // Class
  def member[B >: A <% Ordered[B]](value: B): Boolean
  // .end
}

// | *Set empty
case object Leaf extends Set[Nothing]
// .end
{
  // | *insert
  // Leaf
  def insert[A <% Ordered[A]](value: A): Set[A] =
    new Node(Leaf, value, Leaf)
  // .end
  // | *member
  // Leaf
  def member[A <% Ordered[A]](anyVal: A) = false
  // .end
}

// | *Set
case class Node[A <% Ordered[A]](left: Set[A],
                                 value: A,
                                 right: Set[A])
    extends Set[A]
// .end
{
  // | *insert
  // Node
  def insert[B >: A <% Ordered[B]](newVal: B): Set[B] =
    if(newVal < value)
      new Node(left.insert(newVal), value, right)
    else if(newVal > value)
      new Node(left, value, right.insert(newVal))
    else
      this
  // .end

  // | *member
  // Node
  def member[B >: A <% Ordered[B]](valToCheck: B): Boolean =
    if(valToCheck < value)
      left.member(valToCheck)
    else if(valToCheck > value)
      right.member(valToCheck)
    else
      true
  // .end
}
