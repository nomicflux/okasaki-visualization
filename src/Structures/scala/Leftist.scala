package okasaki.leftist;

object Leftist
{
  // | *merge insert deleteMin
  // Static
  def singleton[A <% Ordered[A]](value: A): Leftist[A] =
    new Node(Leaf, value, Leaf, 1)
  // .end

  // | *merge insert deleteMin
  // Static
  def makeNode[A <% Ordered[A]](newVal: A,
                                        aHeap: Leftist[A],
                                        bHeap: Leftist[A]): Leftist[A] =
    if(aHeap.rank >= bHeap.rank)
      new Node(aHeap, newVal, bHeap, bHeap.rank + 1)
    else
      new Node(bHeap, newVal, aHeap, aHeap.rank + 1)
  // .end
}

// | *LeftistHeap
sealed abstract class Leftist[+A <% Ordered[A]]
// .end
{
  // | *merge insert deleteMin
  // Class
  def merge[B >: A <% Ordered[B]](other: Leftist[B]): Leftist[B]
  // .end
  // | *insert
  // Class
  def insert[B >: A <% Ordered[B]](value: B): Leftist[B] =
    this.merge(Leftist.singleton(value))
  // .end
  // | *deleteMin
  // Class
  def deleteMin: Leftist[A]
  // .end
  // | *findMin
  // Class
  def findMin: A
  // .end
  // | *rank
  // Class
  def rank: Int
  // .end
}

// | *LeftistHeap empty
case object Leaf extends Leftist[Nothing]
// .end
{
  // | *deleteMin
  // Leaf
  def deleteMin = throw new NoSuchElementException("Leaf.deleteMin")
  // .end
  // | *findMin
  // Leaf
  def findMin = throw new NoSuchElementException("Leaf.findMin")
  // .end
  // | *merge insert deleteMin
  // Leaf
  def merge[A <% Ordered[A]](other: Leftist[A]): Leftist[A] = other
  // .end
  // | *rank
  // Leaf
  val rank = 0
  // .end
}

// | *LeftistHeap rank
case class Node[A <% Ordered[A]](left: Leftist[A],
                                 value: A,
                                 right: Leftist[A],
                                 rank: Int) extends Leftist[A]
// .end
{
  // | *findMin
  // Node
  def findMin: A = value
  // .end
  // | *deleteMin
  // Node
  def deleteMin: Leftist[A] = left.merge(right)
  // .end
  // | *merge insert deleteMin
  // Node
  def merge[B >: A <% Ordered[B]](other: Leftist[B]): Leftist[B] =
    other match {
      case Leaf => this
      case Node(l,v,r,n) =>
        if(value <= v)
          Leftist.makeNode(value, left, right.merge(other))
        else
          Leftist.makeNode(v, l, this.merge(r))
    }
  // .end
}
