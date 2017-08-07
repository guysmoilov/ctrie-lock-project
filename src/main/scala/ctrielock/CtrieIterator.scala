package ctrielock

import scala.collection.{GenMap}

/**
  * <p>
  * Created by Tolstoyevsky on 05/08/2017.
  * </p>
  */
class CtrieIterator[K, V](ct: ConcurrentTrie[K, V], mustInit: Boolean = true)
  extends scala.collection.Iterator[(K, V)] {
  var stack = new Array[Array[BasicNode]](7)
  var stackpos = new Array[Int](7)
  var depth = -1
  var subiter: Iterator[(K, V)] = null
  var current: KVNode[K, V] = null

  if (mustInit) initialize()

  def hasNext = (current ne null) || (subiter ne null)

  def next() = if (hasNext) {
    var r: (K, V) = null
    if (subiter ne null) {
      r = subiter.next()
      checkSubiter()
    } else {
      r = current.kvPair
      advance()
    }
    r
  } else Iterator.empty.next()

  private def readin(in: INode[K, V]) = in.READ_MAIN() match {
    case cn: CNode[K, V] =>
      depth += 1
      stack(depth) = cn.array
      stackpos(depth) = -1
      advance()
    case tn: TNode[K, V] =>
      current = tn
    case ln: LNode[K, V] =>
      subiter = ln.listmap.iterator
      checkSubiter()
    case null =>
      current = null
  }

  @inline private def checkSubiter() = if (!subiter.hasNext) {
    subiter = null
    advance()
  }

  @inline private def initialize() {
    assert(ct.isReadOnly)

    val r = ct.READ_ROOT()
    readin(r)
  }

  def advance(): Unit = if (depth >= 0) {
    val npos = stackpos(depth) + 1
    if (npos < stack(depth).length) {
      stackpos(depth) = npos
      stack(depth)(npos) match {
        case sn: SNode[K, V] =>
          current = sn
        case in: INode[K, V] =>
          readin(in)
      }
    } else {
      depth -= 1
      advance()
    }
  } else current = null

  /** Returns a sequence of iterators over subsets of this iterator.
   *  It's used to ease the implementation of splitters for a parallel version of the Ctrie.
   */
  protected def subdivide: Seq[Iterator[(K, V)]] = if (subiter ne null) {
    // the case where an LNode is being iterated
    val it = subiter
    subiter = null
    advance()
    Seq(it, this)
  } else if (depth == -1) Seq(this) else {
    var d = 0
    while (d <= depth) {
      val rem = stack(d).length - 1 - stackpos(d)
      if (rem > 0) {
        val (arr1, arr2) = stack(d).drop(stackpos(d) + 1).splitAt(rem / 2)
        stack(d) = arr1
        stackpos(d) = -1
        val it = new CtrieIterator[K, V](ct, false)
        it.stack(0) = arr2
        it.stackpos(0) = -1
        it.depth = 0
        it.advance() // <-- fix it
        return Seq(this, it)
      }
      d += 1
    }
    Seq(this)
  }

  private def print {
    println("ctrie iterator")
    println(stackpos.mkString(","))
    println("depth: " + depth)
    println("curr.: " + current)
    println(stack.mkString("\n"))
  }

  // Forced to add these by the compiler, probably added in a newer version of scala
  override def toArray[A1 >: (K, V)](implicit evidence$1: ClassManifest[A1]): Array[A1] = toArray

  override def toMap[K, V](implicit ev: <:<[(K, V), (K, V)]): GenMap[K, V] = ??? // TODO: implement
}
