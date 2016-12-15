package okasaki.leftist;

// | *LeftistHeap
object Leftist
// .end
{
  // | *empty
  def empty[A <% Ordered[A]]: Leftist[A] = Leaf
  // .end
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
  def merge[B >: A <% Ordered[B]](other: Leftist[B]): Leftist[B] =
    this match {
      case Leaf => other
      case Node(left,value,right,rank) =>
        other match {
          case Leaf => this
          case Node(l,v,r,n) =>
            if(value <= v)
              Leftist.makeNode(value, left, right.merge(other))
            else
              Leftist.makeNode(v, l, this.merge(r))
        }
    }
  // .end
  // | *insert
  def insert[B >: A <% Ordered[B]](value: B): Leftist[B] =
    this.merge(Leftist.singleton(value))
  // .end
  // | *deleteMin
  def deleteMin: Leftist[A] = this match {
    case Leaf => throw new NoSuchElementException("Leaf.deleteMin")
    case Node(l,_,r,_) => l.merge(r)
  }
  // .end
  // | *findMin
  def findMin: A = this match {
    case Leaf => throw new NoSuchElementException("Leaf.findMin")
    case Node(_,v,_,_) => v
  }
  // .end
  // | *rank
  // Leaf
  val rank: Int = 0
  // .end
}

// | *LeftistHeap empty
case object Leaf extends Leftist[Nothing]
// .end

// | *LeftistHeap rank
case class Node[A <% Ordered[A]](left: Leftist[A],
                                 value: A,
                                 right: Leftist[A],
                                 override val rank: Int) extends Leftist[A]
// .end
