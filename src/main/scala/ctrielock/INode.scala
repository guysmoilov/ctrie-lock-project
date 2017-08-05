package ctrielock

import scala.annotation.tailrec

/**
  * <p>
  * Created by Tolstoyevsky on 05/08/2017.
  * </p>
  */
final class INode[K, V](bn: MainNode[K, V], g: Gen) extends INodeBase[K, V](g) {
  import INodeBase._

  WRITE(bn)

  def this(g: Gen) = this(null, g)

  @Deprecated
  @inline final def WRITE(nval: MainNode[K, V]) = INodeBase.updater.set(this, nval)

  @Deprecated
  @inline final def CAS(old: MainNode[K, V], n: MainNode[K, V]) = INodeBase.updater.compareAndSet(this, old, n)

  @Deprecated
  @inline final def GCAS_READ(ct: ConcurrentTrie[K, V]): MainNode[K, V] = {
    val m = /*READ*/mainnode
    val prevval = /*READ*/m.prev
    if (prevval eq null) m
    else GCAS_Complete(m, ct)
  }

  @Deprecated
  @tailrec private def GCAS_Complete(m: MainNode[K, V], ct: ConcurrentTrie[K, V]): MainNode[K, V] =
    if (m eq null) null
    else {
      // complete the GCAS
      val prev = /*READ*/ m.prev
      val ctr = ct.RDCSS_READ_ROOT(true)

      prev match {
        case null =>
          m
        case fn: FailedNode[_, _] => // try to commit to previous value
          if (CAS(m, fn.prev)) fn.prev
          else GCAS_Complete(/*READ*/ mainnode, ct)
        case vn: MainNode[_, _] =>
          // Assume that you've read the root from the generation G.
          // Assume that the snapshot algorithm is correct.
          // ==> you can only reach nodes in generations <= G.
          // ==> `gen` is <= G.
          // We know that `ctr.gen` is >= G.
          // ==> if `ctr.gen` = `gen` then they are both equal to G.
          // ==> otherwise, we know that either `ctr.gen` > G, `gen` < G,
          //     or both
          if ((ctr.gen eq gen) && ct.nonReadOnly) {
            // try to commit
            if (m.CAS_PREV(prev, null)) m
            else GCAS_Complete(m, ct)
          } else {
            // try to abort
            m.CAS_PREV(prev, new FailedNode(prev))
            GCAS_Complete(/*READ*/ mainnode, ct)
          }
      }
    }

  @Deprecated
  @inline final def GCAS(old: MainNode[K, V], n: MainNode[K, V], ct: ConcurrentTrie[K, V]): Boolean = {
    n.WRITE_PREV(old)
    if (CAS(old, n)) {
      GCAS_Complete(n, ct)
      /*READ*/n.prev eq null
    } else false
  }

  @inline private def inode(cn: MainNode[K, V]) = {
    val nin = new INode[K, V](gen)
    nin.WRITE(cn)
    nin
  }

  @inline final def copyToGen(ngen: Gen, ct: ConcurrentTrie[K, V]) = {
    val nin = new INode[K, V](ngen)
    val main = GCAS_READ(ct)
    nin.WRITE(main)
    nin
  }

  /** Inserts a key value pair, overwriting the old pair if the keys match.
   *
   *  @return        true if successful, false otherwise
   */
  @tailrec final def rec_insert(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: ConcurrentTrie[K, V]): Boolean = {
    val m = GCAS_READ(ct) // use -Yinline!

    m match {
      case cn: CNode[K, V] => // 1) a multiway node
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        val mask = flag - 1
        val pos = Integer.bitCount(bmp & mask)
        if ((bmp & flag) != 0) {
          // 1a) insert below
          cn.array(pos) match {
            case in: INode[K, V] =>
              if (startgen eq in.gen)
                // Go down a level in the recursion
                return in.rec_insert(k, v, hc, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct))
                  // Retry current invocation
                  return rec_insert(k, v, hc, lev, parent, startgen, ct)
                else
                  // Retry whole insert operation from root
                  return false
              }
            case sn: SNode[K, V] =>
              if (sn.hc == hc && sn.k == k)
                // Replace the value of the existing SNode
                return GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)
              else {
                // Hash collision, either create a new subtree or put the two values into an LNode.
                val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                return GCAS(cn, nn, ct)
              }
          }
        } else {
          // Current CNode doesn't contain the new key k, so just insert it.
          val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
          val ncnode = rn.insertedAt(pos, flag, new SNode(k, v, hc), gen)
          return GCAS(cn, ncnode, ct)
        }
      case tn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        return false
      case ln: LNode[K, V] =>
        val nn = ln.inserted(k, v)
        return GCAS(ln, nn, ct)
    }
  }

  /** Inserts a new key value pair, given that a specific condition is met.
   *
   *  @param cond        null - don't care if the key was there; KEY_ABSENT - key wasn't there; KEY_PRESENT - key was there; other value `v` - key must be bound to `v`
   *  @return            null if unsuccessful, Option[V] otherwise (indicating previous value bound to the key)
   */
  @tailrec final def rec_insertif(k: K, v: V, hc: Int, cond: AnyRef, lev: Int, parent: INode[K, V], startgen: Gen, ct: ConcurrentTrie[K, V]): Option[V] = {
    val m = GCAS_READ(ct)  // use -Yinline!

    m match {
      case cn: CNode[K, V] => // 1) a multiway node
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        val mask = flag - 1
        val pos = Integer.bitCount(bmp & mask)
        if ((bmp & flag) != 0) {
          // 1a) insert below
          cn.array(pos) match {
            case in: INode[K, V] =>
              if (startgen eq in.gen)
                // Go down one level in the recursion
                return in.rec_insertif(k, v, hc, cond, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct))
                  // Retry current invocation with new Gen
                  return rec_insertif(k, v, hc, cond, lev, parent, startgen, ct)
                else
                  // Fail due to generation change
                  return null
              }
            case sn: SNode[K, V] => cond match {
              case null =>
                if (sn.hc == hc && sn.k == k) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct))
                    // Replaced existing value, return the previous value
                    return Some(sn.v)
                  else
                    // Fail (due to either Gen change or concurrent change)
                    return null
                }
                else {
                  // Hashcode collision
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                  val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                  if (GCAS(cn, nn, ct))
                    // Succesfully inserted, but key did not exist, so no previous value
                    return None
                  else
                  // Fail (due to either Gen change or concurrent change)
                    return null
                }
              case INode.KEY_ABSENT =>
                if (sn.hc == hc && sn.k == k)
                  // Key already exists, return existing value without replacing
                  return Some(sn.v)
                else {
                  // Hashcode collision
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                  val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                  if (GCAS(cn, nn, ct))
                    // Successfully insert new value, no existing value was replaced
                    return None
                  else
                    // Fail (due to either Gen change or concurrent change)
                    return null
                }
              case INode.KEY_PRESENT =>
                if (sn.hc == hc && sn.k == k) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct))
                    // Successfully replaced previous value
                    return Some(sn.v)
                  else
                    // Fail (due to either Gen change or concurrent change)
                    return null
                }
                else
                  // No existing key present, so no need to replace
                  return None
              case otherv: V =>
                if (sn.hc == hc && sn.k == k && sn.v == otherv) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct))
                    // Successfully replaced existing value, return old value
                    return Some(sn.v)
                  else
                    // Fail (due to either Gen change or concurrent change)
                    return null
                }
                else
                  // Expected value or key not found
                  return None
            }
          }
        }
        // If the key doesn't exist in the CNode
        else cond match {
          case null | INode.KEY_ABSENT =>
            val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
            val ncnode = rn.insertedAt(pos, flag, new SNode(k, v, hc), gen)
            if (GCAS(cn, ncnode, ct))
              // Successfully inserted, no previous value
              return None
            else
              // Fail (due to either Gen change or concurrent change)
              return null
          case INode.KEY_PRESENT => return None
          case otherv: V => return None
        }
      case sn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        return null
      case ln: LNode[K, V] => // 3) an l-node
        @inline def insertln() = {
          val nn = ln.inserted(k, v)
          GCAS(ln, nn, ct)
        }
        cond match {
          case null =>
            val optv = ln.get(k)
            if (insertln())
              return optv
            else
              return null
          case INode.KEY_ABSENT =>
            ln.get(k) match {
              case None => if (insertln())
                return None
              else
                return null
              case optv => return optv
            }
          case INode.KEY_PRESENT =>
            ln.get(k) match {
              case Some(v0) => if (insertln())
                return Some(v0)
              else
                return null
              case None => return None
            }
          case otherv: V =>
            ln.get(k) match {
              case Some(v0) if v0 == otherv =>
                if (insertln())
                  return Some(otherv)
                else
                  return null
              case _ => return None
            }
        }
    }
  }

  /** Looks up the value associated with the key.
   *
   *  @return          null if no value has been found, RESTART if the operation wasn't successful, or any other value otherwise
   */
  @tailrec final def rec_lookup(k: K, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: ConcurrentTrie[K, V]): AnyRef = {
    val m = GCAS_READ(ct) // use -Yinline!

    m match {
      case cn: CNode[K, V] => // 1) a multinode
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        if ((bmp & flag) == 0) null // 1a) bitmap shows no binding
        else { // 1b) bitmap contains a value - descend
          val pos = if (bmp == 0xffffffff) idx else Integer.bitCount(bmp & (flag - 1))
          val sub = cn.array(pos)
          sub match {
            case in: INode[K, V] =>
              if (ct.isReadOnly || (startgen eq in.gen))
                // Go down one level in recursion
                return in.rec_lookup(k, hc, lev + 5, this, startgen, ct)
              else {
                // Generation mismatch - we'll probably ignore this in read-only-operations
                if (GCAS(cn, cn.renewed(startgen, ct), ct))
                  // Retry current invocation
                  return rec_lookup(k, hc, lev, parent, startgen, ct)
                else
                  // Failed to update Gen, restart lookup from root
                  return RESTART // used to be throw RestartException
              }
            case sn: SNode[K, V] =>
              if (sn.hc == hc && sn.k == k)
                // Found
                return sn.v.asInstanceOf[AnyRef]
              else
                // Not found
                return null
          }
        }
      case tn: TNode[K, V] =>
        // Here, we might choose not to clean the parent in read-only operations - just check the contents of the TNode.
        def cleanReadOnly(tn: TNode[K, V]) = if (ct.nonReadOnly) {
          clean(parent, ct, lev - 5)
          RESTART // used to be throw RestartException
        }
        else {
          if (tn.hc == hc && tn.k == k)
            tn.v.asInstanceOf[AnyRef]
          else null
        }
        return cleanReadOnly(tn)
      case ln: LNode[K, V] =>
        return ln.get(k).asInstanceOf[Option[AnyRef]].orNull
    }
  }

  /** Removes the key associated with the given value.
   *
   *  @param v         if null, will remove the key irregardless of the value; otherwise removes only if binding contains that exact key and value
   *  @return          null if not successful, an Option[V] indicating the previous value otherwise
   */
  final def rec_remove(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: ConcurrentTrie[K, V]): Option[V] = {
    val m = GCAS_READ(ct) // use -Yinline!

    m match {
      case cn: CNode[K, V] =>
        val idx = (hc >>> lev) & 0x1f
        val bmp = cn.bitmap
        val flag = 1 << idx
        if ((bmp & flag) == 0) None
        else {
          val pos = Integer.bitCount(bmp & (flag - 1))
          val sub = cn.array(pos)
          // The following expression has no "return" commands, because the result is put into "res" instead of exiting
          // the whole method.
          val res = sub match {
            case in: INode[K, V] =>
              if (startgen eq in.gen)
                // Go down one level in recursion
                in.rec_remove(k, v, hc, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct))
                  // Succesfully updated the generation, retry current invocation
                  rec_remove(k, v, hc, lev, parent, startgen, ct)
                else
                  // Failed generation chane, return failure (will retry from root)
                  null
              }
            case sn: SNode[K, V] =>
              if (sn.hc == hc && sn.k == k && (v == null || sn.v == v)) {
                val ncn = cn.removedAt(pos, flag, gen).toContracted(lev)
                if (GCAS(cn, ncn, ct))
                  // Found and removed the key, return the previous value
                  Some(sn.v)
                else
                  // Failed to remove the value, either due to Gen change or concurrent changes, return failure (retry from root)
                  null
              }
              // Key to remove was not found, return
              else None
          }

          // If removal did nothing (key was not found) or failed completely, then the tree is not changed and there's
          // no reason to cleanup
          if (res == None || (res eq null))
            return res
          else {
            // The remove operation succeeded, we may need to clean (shrink) the subtree into the parent's CNode
            @tailrec def cleanParent(nonlive: AnyRef) {
              val pm = parent.GCAS_READ(ct)
              pm match {
                case cn: CNode[K, V] =>
                  val idx = (hc >>> (lev - 5)) & 0x1f
                  val bmp = cn.bitmap
                  val flag = 1 << idx
                  if ((bmp & flag) == 0) {} // somebody already removed this i-node, we're done
                  else {
                    val pos = Integer.bitCount(bmp & (flag - 1))
                    val sub = cn.array(pos)
                    if (sub eq this) nonlive match {
                      case tn: TNode[K, V] =>
                        // Will shrink TNodes under cn into cn, and if cn contains only one SNode after the operation,
                        // toContracted will return a TNode instead of a CNode. (which will cause the parent's parent shrink also)
                        val ncn = cn.updatedAt(pos, tn.copyUntombed, gen).toContracted(lev - 5)
                        if (!parent.GCAS(cn, ncn, ct))
                          // Retry cleanup, maybe the Gen changed or there was a concurrent modification
                          if (ct.RDCSS_READ_ROOT().gen == startgen) return cleanParent(nonlive)
                    }
                  }
                case _ => return // parent is no longer a cnode, we're done (some other thread already cleaned up)
              }
            }

            if (parent ne null) { // never tomb at root
              val n = GCAS_READ(ct)
              if (n.isInstanceOf[TNode[_, _]])
                cleanParent(n)
            }

            // After cleanup, we still nedd to return the actual value we removed.
            return res
          }
        }
      case tn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        // Retry from root
        return null
      case ln: LNode[K, V] =>
        if (v == null) {
          val optv = ln.get(k)
          val nn = ln.removed(k)
          if (GCAS(ln, nn, ct))
            return optv
          else
            return null
        }
        else ln.get(k) match {
          case optv@Some(v0) if v0 == v =>
            val nn = ln.removed(k)
            if (GCAS(ln, nn, ct))
              // Successfully removed from LNode
              return optv
            else
              // Failed either due to Gen change or concurrent modification, retry from root
              return null
          case _ =>
            // LNode doesn't contain the key, nothing to remove
            return None
        }
    }
  }

  private def clean(nd: INode[K, V], ct: ConcurrentTrie[K, V], lev: Int) {
    // TODO: Lock nd, then do this
    val m = nd.GCAS_READ(ct)
    m match {
      case cn: CNode[K, V] => nd.GCAS(cn, cn.toCompressed(ct, lev, gen), ct)
      case _ =>
    }
  }

  /* this is a quiescent method! */
  def string(lev: Int) = "%sINode -> %s".format("  " * lev, mainnode match {
    case null => "<null>"
    case tn: TNode[_, _] => "TNode(%s, %s, %d, !)".format(tn.k, tn.v, tn.hc)
    case cn: CNode[_, _] => cn.string(lev)
    case ln: LNode[_, _] => ln.string(lev)
    case x => "<elem: %s>".format(x)
  })
}

object INode {
  val KEY_PRESENT = new AnyRef
  val KEY_ABSENT = new AnyRef

  def newRootNode[K, V] = {
    val gen = new Gen
    val cn = new CNode[K, V](0, new Array(0), gen)
    new INode[K, V](cn, gen)
  }
}