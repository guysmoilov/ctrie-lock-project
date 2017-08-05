package ctrielock

import scala.annotation.tailrec
import scala.collection.{Map, mutable, immutable}

class ConcurrentTrie[K, V] private (rt: INode[K,V], isReadOnly : Boolean)
extends mutable.ConcurrentMap[K, V] {

  private val rootLock = new Object
  @volatile private var root = rt

  def this(r: INode[K,V]) = this(r, false)

  def this() = this(INode.newRootNode : INode[K,V])
  
  /* internal methods */

  @inline private def READ_ROOT() = {
    root
  }

  @inline private def WRITE_ROOT(newR : INode[K,V]) = {
    root = newR
  }
  
  @tailrec private def inserthc(k: K, hc: Int, v: V) {
    val r = READ_ROOT()
    if (!r.rec_insert(k, v, hc, 0, null, r.gen, this)) inserthc(k, hc, v)
  }
  
  @tailrec private def insertifhc(k: K, hc: Int, v: V, cond: AnyRef): Option[V] = {
    val r = READ_ROOT()
    val ret = r.rec_insertif(k, v, hc, cond, 0, null, r.gen, this)
    if (ret eq null) insertifhc(k, hc, v, cond)
    else ret
  }
  
  @tailrec private def lookuphc(k: K, hc: Int): AnyRef = {
    val r = READ_ROOT()
    val res = r.rec_lookup(k, hc, 0, null, r.gen, this)
    if (res eq INode.RESTART) lookuphc(k, hc)
    else res
  }
  
  @tailrec private def removehc(k: K, v: V, hc: Int): Option[V] = {
    val r = READ_ROOT()
    val res = r.rec_remove(k, v, hc, 0, null, r.gen, this)
    if (res ne null) res
    else removehc(k, v, hc)
  }
  
  def string = READ_ROOT().string(0)
  
  /* public methods */
  
  @inline final def nonReadOnly = !isReadOnly

  final def snapshot(): ConcurrentTrie[K, V] = {
    doSnapshotSync(false)
  }

  final def readOnlySnapshot(): Map[K, V] = {
    doSnapshotSync(true)
  }

  final override def clear() {
    // I think this should be enough... need to think about it carefully
    rootLock.synchronized {
      WRITE_ROOT(INode.newRootNode)
    }
  }

  final def doSnapshotSync(readonlySnapshot : Boolean): ConcurrentTrie[K, V] = {
    rootLock.synchronized {
      // Can safely use currRoot in the rest of this method body, since we locked the CTrie and no one can change the root
      // except the current thread
      val currRoot = READ_ROOT()
      // Also lock on the root itself to prevent a thread modifying its main node while we're copying it.
      currRoot.synchronized {
        val newRoot = currRoot.copyToGen(new Gen, this)
        // If the snapshot we return is destined to be readOnly, we can reuse the current root, since we know the snapshot
        // won't be mutating it, and mutations on "this" will occur on the new generation root
        val snapshotRoot = if (readonlySnapshot) currRoot else currRoot.copyToGen(new Gen, this)
        WRITE_ROOT(newRoot) // Linearization point
        return new ConcurrentTrie[K, V](snapshotRoot, readonlySnapshot)
      }
    }
  }

  final def lookup(k: K): V = {
    val hc = ConcurrentTrie.computeHash(k)
    lookuphc(k, hc).asInstanceOf[V]
  }
  
  final override def apply(k: K): V = {
    val hc = ConcurrentTrie.computeHash(k)
    val res = lookuphc(k, hc)
    if (res eq null) throw new NoSuchElementException
    else res.asInstanceOf[V]
  }
  
  final def get(k: K): Option[V] = {
    val hc = ConcurrentTrie.computeHash(k)
    Option(lookuphc(k, hc)).asInstanceOf[Option[V]]
  }
  
  override def put(key: K, value: V): Option[V] = {
    val hc = ConcurrentTrie.computeHash(key)
    insertifhc(key, hc, value, null)
  }
  
  final override def update(k: K, v: V) {
    val hc = ConcurrentTrie.computeHash(k)
    inserthc(k, hc, v)
  }
  
  final def +=(kv: (K, V)) = {
    update(kv._1, kv._2)
    this
  }
  
  final override def remove(k: K): Option[V] = {
    val hc = ConcurrentTrie.computeHash(k)
    removehc(k, null.asInstanceOf[V], hc)
  }
  
  final def -=(k: K) = {
    remove(k)
    this
  }
  
  def putIfAbsent(k: K, v: V): Option[V] = {
    val hc = ConcurrentTrie.computeHash(k)
    insertifhc(k, hc, v, INode.KEY_ABSENT)
  }
  
  def remove(k: K, v: V): Boolean = {
    val hc = ConcurrentTrie.computeHash(k)
    removehc(k, v, hc).nonEmpty
  }
  
  def replace(k: K, oldvalue: V, newvalue: V): Boolean = {
    val hc = ConcurrentTrie.computeHash(k)
    insertifhc(k, hc, newvalue, oldvalue.asInstanceOf[AnyRef]).nonEmpty
  }
  
  def replace(k: K, v: V): Option[V] = {
    val hc = ConcurrentTrie.computeHash(k)
    insertifhc(k, hc, v, INode.KEY_PRESENT)
  }
  
  def iterator: Iterator[(K, V)] =
    if (nonReadOnly) readOnlySnapshot().iterator
    else new CtrieIterator(this)

  override def toArray[A1 >: (K, V)](implicit evidence$1: ClassManifest[A1]): Array[A1] = toArray

  override def toIndexedSeq[A1 >: (K, V)]: immutable.IndexedSeq[A1] = null
}

object ConcurrentTrie {
  @inline final def computeHash[K](k: K): Int = {
    k.hashCode
  }
}