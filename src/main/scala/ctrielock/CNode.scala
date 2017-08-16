package ctrielock

/**
  * <p>
  * Created by Tolstoyevsky on 05/08/2017.
  * </p>
  */
final class CNode[K, V](final val bitmap: Int, final val array: Array[BasicNode], final val gen: Gen)
extends MainNode[K, V] {

  /**
    * @return (does the given hash exist in this CNode,
    *         the flag for the hash in the bitmap,
    *         the position of the target node in the CNode's array (meaningless if not found!) )
    */
  final def findPositions(hc: Int, lev: Int) : (Boolean,Int,Int) = {
    val idx = (hc >>> lev) & 0x1f
    val flag = 1 << idx
    val mask = flag - 1
    val pos = Integer.bitCount(bitmap & mask)
    val wasFound = (bitmap & flag) != 0
    (wasFound, flag, pos)
  }

  final def getElementAt(hc: Int, lev: Int): Option[BasicNode] = {
    val (wasFound,flag,pos) = findPositions(hc,lev)
    if (wasFound)
      Option(array(pos))
    else None
  }

  final def updatedAt(pos: Int, nn: BasicNode, gen: Gen) = {
    val len = array.length
    val narr = new Array[BasicNode](len)
    Array.copy(array, 0, narr, 0, len)
    narr(pos) = nn
    new CNode[K, V](bitmap, narr, gen)
  }

  final def removedAt(pos: Int, flag: Int, gen: Gen) = {
    val arr = array
    val len = arr.length
    val narr = new Array[BasicNode](len - 1)
    Array.copy(arr, 0, narr, 0, pos)
    Array.copy(arr, pos + 1, narr, pos, len - pos - 1)
    new CNode[K, V](bitmap ^ flag, narr, gen)
  }

  final def insertedAt(pos: Int, flag: Int, nn: BasicNode, gen: Gen) = {
    val len = array.length
    val bmp = bitmap
    val narr = new Array[BasicNode](len + 1)
    Array.copy(array, 0, narr, 0, pos)
    narr(pos) = nn
    Array.copy(array, pos, narr, pos + 1, len - pos)
    new CNode[K, V](bmp | flag, narr, gen)
  }

  /** Returns a copy of this cnode such that all the i-nodes below it are copied
   *  to the specified generation `ngen`.
   */
  final def renewed(ngen: Gen) = {
    var i = 0
    val arr = array
    val len = arr.length
    val narr = new Array[BasicNode](len)
    while (i < len) {
      arr(i) match {
        case in: INode[K, V] => narr(i) = in.copyToGen(ngen)
        case bn: BasicNode => narr(i) = bn
      }
      i += 1
    }
    new CNode[K, V](bitmap, narr, ngen)
  }

  private def resurrect(inode: INode[K, V], inodemain: AnyRef) = inodemain match {
    case tn: TNode[_, _] => tn.copyUntombed
    case _ => inode
  }

  final def toContracted(lev: Int) = if (array.length == 1 && lev > 0) array(0) match {
    case sn: SNode[K, V] => sn.copyTombed
    case _ => this
  } else this

  // - if the branching factor is 1 for this CNode, and the child
  //   is a tombed SNode, returns its tombed version
  // - otherwise, if there is at least one non-null node below,
  //   returns the version of this node with at least some null-inodes
  //   removed (those existing when the op began)
  // - if there are only null-i-nodes below, returns null
  final def toCompressed(lev: Int, gen: Gen) = {
    var bmp = bitmap
    var i = 0
    val arr = array
    val tmparray = new Array[BasicNode](arr.length)
    while (i < arr.length) { // construct new bitmap
      val sub = arr(i)
      sub match {
        case in: INode[K, V] =>
          val inodemain = in.READ_MAIN()
          assert(inodemain ne null)
          tmparray(i) = resurrect(in, inodemain)
        case sn: SNode[K, V] =>
          tmparray(i) = sn
      }
      i += 1
    }

    new CNode[K, V](bmp, tmparray, gen).toContracted(lev)
  }

  private[ctrielock] def string(lev: Int): String = "CNode %x\n%s".format(bitmap, array.map(_.string(lev + 1)).mkString("\n"))

  /* quiescently consistent - don't call concurrently to anything involving a GCAS!! */
  protected def collectElems: Seq[(K, V)] = array flatMap {
    case sn: SNode[K, V] => Some(sn.kvPair)
    case in: INode[K, V] => in.READ_MAIN() match {
      case tn: TNode[K, V] => Some(tn.kvPair)
      case ln: LNode[K, V] => ln.listmap.toList
      case cn: CNode[K, V] => cn.collectElems
    }
  }

  protected def collectLocalElems: Seq[String] = array flatMap {
    case sn: SNode[K, V] => Some(sn.kvPair._2.toString)
    case in: INode[K, V] => Some(in.toString.drop(14) + "(" + in.gen + ")")
  }

  override def toString = {
    val elems = collectLocalElems
    "CNode(sz: %d; %s)".format(elems.size, elems.sorted.mkString(", "))
  }
}

object CNode {

  /**
    * Used when we detect a hash collision (xhc == yhc) for two different keys (x.key != y.key)
    * If we can create a new subtree under the current CNode (lev < 35), then we'll do that.
    * Otherwise, return a new LNode.
    */
  def dual[K, V](x: SNode[K, V], xhc: Int, y: SNode[K, V], yhc: Int, lev: Int, gen: Gen): MainNode[K, V] = {
    if (lev < 35) {
      val xidx = (xhc >>> lev) & 0x1f
      val yidx = (yhc >>> lev) & 0x1f
      val bmp = (1 << xidx) | (1 << yidx)
      if (xidx == yidx) {
        val subinode = new INode[K, V](gen) //(ConcurrentTrie.inodeupdater)
        subinode.mainnode = dual(x, xhc, y, yhc, lev + 5, gen)
        return new CNode(bmp, Array(subinode), gen)
      } else {
        if (xidx < yidx)
          return new CNode(bmp, Array(x, y), gen)
        else
          return new CNode(bmp, Array(y, x), gen)
      }
    }
    else {
      return new LNode(x.k, x.v, y.k, y.v)
    }
  }
}