package ctrielock

import java.util.concurrent.atomic._

import scala.annotation.tailrec
//import scala.collection.Map
import scala.collection.concurrent.Map



@Deprecated
case class RDCSS_Descriptor[K, V](old: INode[K, V], expectedmain: MainNode[K, V], nv: INode[K, V]) {
  @volatile var committed = false
}

class ConcurrentTrie[K, V] private (r: AnyRef, rtupd: AtomicReferenceFieldUpdater[ConcurrentTrie[K, V], AnyRef])
extends Map[K, V]
{
  import ConcurrentTrie.computeHash

  @Deprecated
  private val rootupdater = rtupd
  @volatile var root = r
  
  def this() = this(
    INode.newRootNode,
    AtomicReferenceFieldUpdater.newUpdater(classOf[ConcurrentTrie[K, V]], classOf[AnyRef], "root")
  )
  
  /* internal methods */

  @Deprecated
  @inline final def CAS_ROOT(ov: AnyRef, nv: AnyRef) = rootupdater.compareAndSet(this, ov, nv)

  @Deprecated
  @inline final def RDCSS_READ_ROOT(abort: Boolean = false): INode[K, V] = {
    val r = /*READ*/root
    r match {
      case in: INode[K, V] => in
      case desc: RDCSS_Descriptor[K, V] => RDCSS_Complete(abort)
    }
  }

  @Deprecated
  @tailrec private def RDCSS_Complete(abort: Boolean): INode[K, V] = {
    val v = /*READ*/root
    v match {
      case in: INode[K, V] => in
      case desc: RDCSS_Descriptor[K, V] =>
        val RDCSS_Descriptor(ov, exp, nv) = desc
        if (abort) {
          if (CAS_ROOT(desc, ov)) ov
          else RDCSS_Complete(abort)
        } else {
          val oldmain = ov.GCAS_READ(this)
          if (oldmain eq exp) {
            if (CAS_ROOT(desc, nv)) {
              desc.committed = true
              nv
            } else RDCSS_Complete(abort)
          } else {
            if (CAS_ROOT(desc, ov)) ov
            else RDCSS_Complete(abort)
          }
        }
    }
  }

  @Deprecated
  private def RDCSS_ROOT(ov: INode[K, V], expectedmain: MainNode[K, V], nv: INode[K, V]): Boolean = {
    val desc = RDCSS_Descriptor(ov, expectedmain, nv)
    if (CAS_ROOT(ov, desc)) {
      RDCSS_Complete(false)
      /*READ*/desc.committed
    } else false
  }
  
  @tailrec private def inserthc(k: K, hc: Int, v: V) {
    val r = RDCSS_READ_ROOT()
    if (!r.rec_insert(k, v, hc, 0, null, r.gen, this)) inserthc(k, hc, v)
  }
  
  @tailrec private def insertifhc(k: K, hc: Int, v: V, cond: AnyRef): Option[V] = {
    val r = RDCSS_READ_ROOT()
    
    val ret = r.rec_insertif(k, v, hc, cond, 0, null, r.gen, this)
    if (ret eq null) insertifhc(k, hc, v, cond)
    else ret
  }
  
  @tailrec private def lookuphc(k: K, hc: Int): AnyRef = {
    val r = RDCSS_READ_ROOT()
    val res = r.rec_lookup(k, hc, 0, null, r.gen, this)
    if (res eq INode.RESTART) lookuphc(k, hc)
    else res
  }
  
  @tailrec private def removehc(k: K, v: V, hc: Int): Option[V] = {
    val r = RDCSS_READ_ROOT()
    val res = r.rec_remove(k, v, hc, 0, null, r.gen, this)
    if (res ne null) res
    else removehc(k, v, hc)
  }
  
  def string = RDCSS_READ_ROOT().string(0)
  
  /* public methods */
  
  @inline final def isReadOnly = rootupdater eq null
  
  @inline final def nonReadOnly = rootupdater ne null

  @Deprecated
  @tailrec final def snapshot(): ConcurrentTrie[K, V] = {
    val r = RDCSS_READ_ROOT()
    val expmain = r.GCAS_READ(this)
    if (RDCSS_ROOT(r, expmain, r.copyToGen(new Gen, this))) new ConcurrentTrie(r.copyToGen(new Gen, this), rootupdater)
    else snapshot()
  }

  @Deprecated
  @tailrec final def readOnlySnapshot(): Map[K, V] = {
    val r = RDCSS_READ_ROOT()
    val expmain = r.GCAS_READ(this)
    if (RDCSS_ROOT(r, expmain, r.copyToGen(new Gen, this))) new ConcurrentTrie(r, null)
    else readOnlySnapshot()
  }

  @Deprecated
  @tailrec final override def clear() {
    val r = RDCSS_READ_ROOT()
    if (!RDCSS_ROOT(r, r.GCAS_READ(this), INode.newRootNode[K, V])) clear()
  }

  final def doSnapshotSync(snapshotRootUpdater: AtomicReferenceFieldUpdater[ConcurrentTrie[K, V], AnyRef]): ConcurrentTrie[K, V] = {
    this.synchronized {
      // Can safely use currRoot in the rest of this method body, since we locked the CTrie and no one can change the root
      // except the current thread
      val currRootTmp = root
      currRootTmp match {
        case currRoot : INode[K,V] =>
          currRoot.synchronized {
            val newRoot = currRoot.copyToGen(new Gen, this)
            val snapshotRoot = currRoot.copyToGen(new Gen, this)
            this.root = newRoot
            return new ConcurrentTrie[K, V](snapshotRoot, snapshotRootUpdater)
          }
        case _ => throw new IllegalArgumentException // TODO: Make root always an indoe
      }
    }
  }

  final def snapshotSync(): ConcurrentTrie[K, V] = {
    doSnapshotSync(rootupdater)
  }

  final def readOnlySnapshotSync(): Map[K, V] = {
    doSnapshotSync(null)
  }

  final def clearSync() = {
    this.synchronized(
      root = INode.newRootNode[K,V]
    )
  }

  final def lookup(k: K): V = {
    val hc = computeHash(k)
    lookuphc(k, hc).asInstanceOf[V]
  }
  
  final override def apply(k: K): V = {
    val hc = computeHash(k)
    val res = lookuphc(k, hc)
    if (res eq null) throw new NoSuchElementException
    else res.asInstanceOf[V]
  }
  
  final def get(k: K): Option[V] = {
    val hc = computeHash(k)
    Option(lookuphc(k, hc)).asInstanceOf[Option[V]]
  }
  
  override def put(key: K, value: V): Option[V] = {
    val hc = computeHash(key)
    insertifhc(key, hc, value, null)
  }
  
  final override def update(k: K, v: V) {
    val hc = computeHash(k)
    inserthc(k, hc, v)
  }
  
  final def +=(kv: (K, V)) = {
    update(kv._1, kv._2)
    this
  }
  
  final override def remove(k: K): Option[V] = {
    val hc = computeHash(k)
    removehc(k, null.asInstanceOf[V], hc)
  }
  
  final def -=(k: K) = {
    remove(k)
    this
  }
  
  def putIfAbsent(k: K, v: V): Option[V] = {
    val hc = computeHash(k)
    insertifhc(k, hc, v, INode.KEY_ABSENT)
  }
  
  def remove(k: K, v: V): Boolean = {
    val hc = computeHash(k)
    removehc(k, v, hc).nonEmpty
  }
  
  def replace(k: K, oldvalue: V, newvalue: V): Boolean = {
    val hc = computeHash(k)
    insertifhc(k, hc, newvalue, oldvalue.asInstanceOf[AnyRef]).nonEmpty
  }
  
  def replace(k: K, v: V): Option[V] = {
    val hc = computeHash(k)
    insertifhc(k, hc, v, INode.KEY_PRESENT)
  }
  
  def iterator: Iterator[(K, V)] =
    if (nonReadOnly) readOnlySnapshot().iterator
    else new CtrieIterator(this)
  
}

object ConcurrentTrie {
  @inline final def computeHash[K](k: K): Int = {
    k.hashCode
  }
}