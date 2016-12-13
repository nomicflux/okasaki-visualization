package okasaki.leftist;

// | *LeftistHeap empty merge insert deleteMin findMin
sealed abstract class Leftist[+A <% Ordered[A]]
// .end
{
  // | *merge insert deleteMin
  private def singleton[A <% Ordered[A]](value: A): Leftist[A] =
    new Node(Leaf, value, Leaf, 1)
  // .end

  // | *merge insert deleteMin
  def merge[B >: A <% Ordered[B]](other: Leftist[B]): Leftist[B]
  // .end
  // | *insert
  def insert[B >: A <% Ordered[B]](value: B): Leftist[B] =
    this.merge(singleton(value))
  // .end
  // | *deleteMin
  def deleteMin: Leftist[A]
  // .end
  // | *findMin
  def findMin: A
  // .end
  // | *rank
  def rank: Int
  // .end
}

// | *LeftistHeap empty
case object Leaf extends Leftist[Nothing]
// .end
{
  // | *deleteMin
  def deleteMin = throw new NoSuchElementException("Leaf.deleteMin")
  // .end
  // | *findMin
  def findMin = throw new NoSuchElementException("Leaf.findMin")
  // .end
  // | *merge insert deleteMin
  def merge[A <% Ordered[A]](other: Leftist[A]): Leftist[A] = other
  // .end
  // | *rank
  def rank = 0
  // .end
}

// | *LeftistHeap rank
case class Node[A <% Ordered[A]](left: Leftist[A],
                                 value: A,
                                 right: Leftist[A],
                                 rank: Int) extends Leftist[A]
// .end
{
  // | *merge insert deleteMin
  private def makeNode[A <% Ordered[A]](newVal: A,
                                 aHeap: Leftist[A],
                                 bHeap: Leftist[A]): Leftist[A] =
   if(aHeap.rank >= bHeap.rank)
     new Node(aHeap, newVal, bHeap, bHeap.rank + 1)
   else
     new Node(bHeap, newVal, aHeap, aHeap.rank + 1)
  // .end

  // | *findMin
  def findMin: A = value
  // .end
  // | *deleteMin
  def deleteMin: Leftist[A] = left.merge(right)
  // .end
  // | *merge insert deleteMin
  def merge[B >: A <% Ordered[B]](other: Leftist[B]): Leftist[B] = other match {
    case Leaf => other
    case Node(l,v,r,n) =>
      if(value <= v)
        makeNode(value, left, right.merge(other))
      else
        makeNode(v, l, this.merge(r))
  }
  // .end
}
