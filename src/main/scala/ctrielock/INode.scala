package ctrielock

import scala.annotation.tailrec

/**
  * <p>
  * Created by Tolstoyevsky on 05/08/2017.
  * </p>
  */
final class INode[K, V](bn: MainNode[K, V], val gen: Gen) extends BasicNode {

  // Copied from INodeBase
  @volatile var mainnode: MainNode[K, V] = bn
  // End

  def this(g: Gen) = this(null, g)

  /**
    * The usage of this field should be something like this:
    * <pre>
    * def someAtomicWriteOperation(ct: CTrie) {
    *   this.synchronized {
    *     val whatToWrite = (something based on the existing mainnode and the specific type of write operation)
    *     <b>aboutToWrite = true</b>
    *     if (ct.READ_ROOT().gen == expectedGen) {
    *
    *       // Context switch could potentially happen here.
    *       // Say a context switch happens here, then a readonly snapshot is taken.
    *       // Readers could then read this.mainnode.
    *       // Next, this thread regains control, and overwrites this.mainnode, since it already compared generations.
    *       // This violates linearizability for the readers.
    *       // The code in READ_MAIN should prevent readers from seeing a value that is about to be
    *       // overwritten in the next line.
    *
    *       WRITE_MAIN(whatToWrite)
    *     }
    *     <b>aboutToWrite = false</b>
    *   }
    * }
    * </pre>
    */
  @volatile private var aboutToWrite = false

  @inline private final def WRITE_MAIN(nval: MainNode[K, V]) = {
    // Important to compile without assertions for benchmarks!
    // Using scalac -Xelide-below MAXIMUM
    assert(aboutToWrite)
    mainnode = nval
  }

  /**
    * The point here is to only return the mainnode from a quiescent point in time - otherwise, a readonly snapshot iterator
    * could interleave with an inserting thread that already decided to overwrite the main node. <br/>
    * (see [[ctrielock.INode.aboutToWrite]] )
    */
  @inline @tailrec private[ctrielock] final def READ_MAIN(): MainNode[K, V] = {
    val tmp = mainnode
    if (!aboutToWrite && mainnode == tmp)
      tmp
    else
      READ_MAIN()
  }

  private final val NO_BACKUP_FUNC : MainNode[K,V] => Option[MainNode[K,V]] = (_) => None

  /**
    * GCAS_SYNC without a backup generator, for when we don't care about failures
    */
  @inline private final def GCAS_SYNC(expected: MainNode[K,V],
                                      newMain: MainNode[K,V],
                                      ct: ConcurrentTrie[K,V]) : Boolean =
    GCAS_SYNC(expected, newMain, ct, NO_BACKUP_FUNC) == GcasSuccess

  /**
    * TODO : Return one of three values here :
    * <ul>
    *   <li>success</li>
    *   <li>failure due to mainnode mismatch (which is recoverable)</li>
    *   <li>failure due to generation mismatch (which is NOT recoverable)</li>
    * </ul>
    *
    * In the second part of the tuple, the actual mainnode that was found is returned
    */
  @inline private final def GCAS_SYNC(expected: MainNode[K,V],
                                      newMain: MainNode[K,V],
                                      ct: ConcurrentTrie[K,V],
                                      backupGen : MainNode[K,V] => Option[MainNode[K,V]]) : GcasResult = {
    this.synchronized {
      // No need to use READ_MAIN(), since we hold the lock
      val currMain = mainnode
      aboutToWrite = true
      if (this.gen != ct.READ_ROOT().gen) {
        aboutToWrite = false
        /*return*/ GcasGenFail
      }
      else {
        if (currMain == expected) {
          WRITE_MAIN(newMain)
          aboutToWrite = false
          /*return*/ GcasSuccess
        }
        else {
          // TODO: I'm worried about 2 things here:
          // 1. Attempting to recover from CAS failure inside the critical section will cause slower performance
          // than releasing the lock and retrying the CAS (might cause a much longer time spent in the critical section)
          // 2. That creating and calling the anonymous functions, and evaluating the Option, will cause a lot of overhead
          // Only benchmarks will tell.
          backupGen(currMain) match {
            case Some(backup) =>
              WRITE_MAIN(backup)
              aboutToWrite = false
              /*return*/ GcasCompareRecovery(currMain)
            case _ =>
              aboutToWrite = false
              /*return*/ GcasCompareFail(currMain)
          }
        }
      }
    }
  }

  @inline def inode(cn: MainNode[K, V]): INode[K, V] = {
     /*return*/ new INode[K, V](cn, gen)
  }

  @inline def copyToGen(ngen: Gen): INode[K, V] = {
    val main = READ_MAIN()
    /*return*/ new INode[K, V](main, ngen)
  }

  @inline private def renewBackupGenerator(startgen: Gen)
  : (MainNode[K, V]) => Option[MainNode[K, V]] = {
    case cn2: CNode[K, V] =>
      if (cn2.gen == startgen) Option(cn2)
      else Option(cn2.renewed(startgen))
    case tn2: TNode[K, V] => None // Requires cleanup anyway
    case ln2: LNode[K, V] => Option(ln2)
  }

  @inline private def attemptGenerationUpdate(cn: CNode[K,V],
                                              startgen: Gen,
                                              ct: ConcurrentTrie[K,V]) : RequiredAction = {
    val renewResult = GCAS_SYNC(cn, cn.renewed(startgen), ct, renewBackupGenerator(startgen))
    renewResult match {
      case GcasGenFail => Restart
      case _ => Retry
    }
  }

  /**
    * More or less repeats the logic in rec_insert, but without GCAS operations.
    * The insertion should fail only if it turns out that:
    * 1) We need to traverse deeper in the tree (CNode at target position contains a subtree)
    * 2) Or the INode is already dead (mainnode == TNode)
    */
  @inline private def insertBackupGenerator(k: K, v: V, hc: Int, lev: Int)
  : (MainNode[K, V]) => Option[MainNode[K, V]] = {
    case tn: TNode[K,V] => /*return*/ None // Cleanup required
    case ln: LNode[K,V] => /*return*/ Option(ln.inserted(k,v))
    case cn: CNode[K,V] =>
      val (wasFound,flag,pos) = cn.findPositions(hc,lev)
      if (wasFound) {
        cn.array(pos) match {
          case in: INode[K, V] => None // Need to exit the critical section and traverse down the tree
          case sn: SNode[K, V] =>
            if (sn.hc == hc && sn.k == k) {
              // Replace existing value
              /*return*/ Option(cn.updatedAt(pos, new SNode(k, v, hc), gen))
            }
            else {
              // Hash collision, create new tree level or LNode
              val oldCnode = if (cn.gen eq gen) cn else cn.renewed(gen)
              val newCnode = CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)
              /*return*/ Option(oldCnode.updatedAt(pos, inode(newCnode), gen))
            }
        }
      }
      else {
        // Current CNode doesn't contain the new key k, so just insert it.
        val rn = if (cn.gen eq gen) cn else cn.renewed(gen)
        /*return*/ Option(rn.insertedAt(pos, flag, new SNode(k, v, hc), gen))
      }
  }

  /** Inserts a key value pair, overwriting the old pair if the keys match.
   *
   *  @return        true if successful, false otherwise
   */
  @tailrec final def rec_insert(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: ConcurrentTrie[K, V]): Boolean = {
    val m = READ_MAIN() // use -Yinline!

    m match {
      case cn: CNode[K, V] => // 1) a multiway node
        val (wasFound,flag,pos) = cn.findPositions(hc,lev)
        if (wasFound) {
          // 1a) insert below
          cn.array(pos) match {
            case in: INode[K, V] =>
              if (startgen eq in.gen)
                // Go down a level in the recursion
                /*return*/ in.rec_insert(k, v, hc, lev + 5, this, startgen, ct)
              else {
                attemptGenerationUpdate(cn, startgen, ct) match {
                  case Retry => rec_insert(k, v, hc, lev, parent, startgen, ct)
                  case _ => /*restart*/ false
                }
              }
            case sn: SNode[K, V] =>
              val newMain = {
                if (sn.hc == hc && sn.k == k) {
                  // Replace the value of the existing SNode
                  cn.updatedAt(pos, new SNode(k, v, hc), gen)
                }
                else {
                  // Hash collision, either create a new subtree or put the two values into an LNode.
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen)
                  rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                }
              }
              // Replace the value of the existing SNode
              attemptInsert(cn, newMain, ct, k, v, hc, lev) match {
                case ReturnExpected | Return(_) => true
                case Retry => rec_insert(k, v, hc, lev, parent, startgen, ct)
                case Restart => false
              }
          }
        }
        else {
          // Current CNode doesn't contain the new key k, so just insert it.
          val rn = if (cn.gen eq gen) cn else cn.renewed(gen)
          val ncnode = rn.insertedAt(pos, flag, new SNode(k, v, hc), gen)
          // Replace the value of the existing SNode
          attemptInsert(cn, ncnode, ct, k, v, hc, lev) match {
            case ReturnExpected | Return(_) => true
            case Retry => rec_insert(k, v, hc, lev, parent, startgen, ct)
            case Restart => false
          }
        }
      case tn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        /*restart*/ false
      case ln: LNode[K, V] =>
        val newList = ln.inserted(k, v)
        // Replace the value of the existing SNode
        attemptInsert(ln, newList, ct, k, v, hc, lev) match {
          case ReturnExpected | Return(_) => true
          case Retry => rec_insert(k, v, hc, lev, parent, startgen, ct)
          case Restart => false
        }
    }
  }

  @inline private def getPreviousValueFromCompareResult(gcasCompareResult: GcasCompareResult[K,V], k: K, hc: Int, lev: Int)
  : PreviousValue = {
    gcasCompareResult.actualMain match {
      case prevCn: CNode[K,V] =>
        val maybePrevNode = prevCn.getElementAt(hc, lev)
        maybePrevNode match {
          case None => NoValue
          case Some(prevNode) =>
            prevNode match {
              // Replaced an existing value
              case prevSn: SNode[K,V]  =>
                if (prevSn.k == k) /*return*/ SomeValue(prevSn.v)
                else /*return*/ NoValue
              case _ => /*return*/ SubTree
            }
        }
      case prevLn: LNode[K,V] =>
        prevLn.get(k) match {
          case None => /*return*/ NoValue
          case Some(v) => /*return*/ SomeValue(v)
        }
      case tn: TNode[K,V] => /*return*/ TombNode
    }
  }

  @inline private final def getPreviousValue(gcasResult: GcasResult, k: K, hc: Int, lev: Int) : PreviousValue = {
    gcasResult match {
      case g @ GcasCompareRecovery(_) =>
        getPreviousValueFromCompareResult(g.asInstanceOf[GcasCompareResult[K,V]], k, hc, lev)
      case g @ GcasCompareFail(_) =>
        getPreviousValueFromCompareResult(g.asInstanceOf[GcasCompareResult[K,V]], k, hc, lev)
      case _ =>
        throw new IllegalStateException("getPreviousValue should never be called with GcasSuccess or GcasGenFail!! " + gcasResult)
    }
  }

  @inline private def insertIfKeyIsAbsentBackupGenerator(k: K, v: V, hc: Int, lev: Int)
  : (MainNode[K, V]) => Option[MainNode[K, V]] = {
    case tn: TNode[K,V] => /*return*/ None // Cleanup required
    case ln: LNode[K,V] =>
      if (ln.listmap.contains(k)) /*return*/ None
      else /*return*/ Option(ln.inserted(k, v))
    case cn: CNode[K,V] =>
      val (wasFound,flag,pos) = cn.findPositions(hc,lev)
      if (wasFound) {
        cn.array(pos) match {
          case in: INode[K, V] => None // Need to exit the critical section and traverse down the tree
          case sn: SNode[K, V] =>
            if (sn.hc == hc && sn.k == k) /*return*/ None
            else {
              // Hash collision, create new tree level or LNode
              val oldCnode = if (cn.gen eq gen) cn else cn.renewed(gen)
              val newCnode = CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)
              /*return*/ Option(oldCnode.updatedAt(pos, inode(newCnode), gen))
            }
        }
      }
      else {
        // Current CNode doesn't contain the new key k, so just insert it.
        val rn = if (cn.gen eq gen) cn else cn.renewed(gen)
        /*return*/ Option(rn.insertedAt(pos, flag, new SNode(k, v, hc), gen))
      }
  }

  @inline private def insertIfKeyIsPresentBackupGenerator(k: K, v: V, hc: Int, lev: Int)
  : (MainNode[K, V]) => Option[MainNode[K, V]] = {
    case tn: TNode[K,V] => /*return*/ None // Cleanup required
    case ln: LNode[K,V] =>
      if (ln.listmap.contains(k)) /*return*/ Option(ln.inserted(k, v))
      else /*return*/ None
    case cn: CNode[K,V] =>
      val (wasFound,flag,pos) = cn.findPositions(hc,lev)
      if (!wasFound) /*return*/ None
      else {
        cn.array(pos) match {
          case in: INode[K, V] => None // Need to exit the critical section and traverse down the tree
          case sn: SNode[K, V] =>
            if (sn.hc == hc && sn.k == k) {
              // Replace existing value
              /*return*/ Option(cn.updatedAt(pos, new SNode(k, v, hc), gen))
            }
            else /*return*/ None
        }
      }
  }

  @inline private def insertIfValueIsPresentBackupGenerator(expectedV: V, k: K, v: V, hc: Int, lev: Int)
  : (MainNode[K, V]) => Option[MainNode[K, V]] = {
    case tn: TNode[K,V] => /*return*/ None // Cleanup required
    case ln: LNode[K,V] =>
      ln.listmap.get(k) match {
        case None => /*return*/ None
        case Some(otherV) if otherV == expectedV => /*return*/ Option(ln.inserted(k,v))
        case _ => /*return*/ None
      }
    case cn: CNode[K,V] =>
      val (wasFound,flag,pos) = cn.findPositions(hc,lev)
      if (!wasFound) /*return*/ None
      else {
        cn.array(pos) match {
          case in: INode[K, V] => None // Need to exit the critical section and traverse down the tree
          case sn: SNode[K, V] =>
            if (sn.hc == hc && sn.k == k && sn.v == expectedV) {
              // Replace existing value
              /*return*/ Option(cn.updatedAt(pos, new SNode(k, v, hc), gen))
            }
            else /*return*/ None
        }
      }
  }

  /**
    * For rec_insertif when cond = null
    */
  @inline private def attemptInsert(expected: MainNode[K,V],
                                    newMain: MainNode[K,V],
                                    ct: ConcurrentTrie[K,V],
                                    k: K, v: V, hc: Int, lev: Int) : RequiredAction = {
    val backupGenerator = insertBackupGenerator(k, v, hc, lev)
    val gcasResult = GCAS_SYNC(expected, newMain, ct, backupGenerator)
    gcasResult match {
      case GcasSuccess => ReturnExpected
      case GcasGenFail => Restart
      case GcasCompareFail(_) => Retry
      case GcasCompareRecovery(_) => /*return*/
        getPreviousValue(gcasResult, k, hc, lev) match {
          case SomeValue(prevV) => Return(Some(prevV))
          case NoValue => /*return*/ Return(None)
          case TombNode | SubTree =>
            assert(assertion = false,
              "Insert can't recover from a subtree or TNode: " + gcasResult)
            Restart
        }
    }
  }

  /**
    * For rec_insertif when cond = KEY_ABSENT
    */
  @inline private def attemptInsertIfKeyAbsent(expected: MainNode[K, V],
                                               newMain: MainNode[K, V],
                                               ct: ConcurrentTrie[K, V],
                                               k: K, v: V, hc: Int, lev: Int): RequiredAction = {
    val backupGenerator = insertIfKeyIsAbsentBackupGenerator(k, v, hc, lev)
    val gcasResult = GCAS_SYNC(expected, newMain, ct, backupGenerator)
    gcasResult match {
      case GcasSuccess => ReturnExpected
      case GcasGenFail => Restart
      case GcasCompareRecovery(_) => Return(None)
      case GcasCompareFail(_) =>
        getPreviousValue(gcasResult, k, hc, lev) match {
          case SomeValue(prevV) => Return(Some(prevV))
          case SubTree | TombNode => Retry
          case NoValue =>
            assert(assertion = false,
              "GCAS should have recovered if the key is absent: " + gcasResult)
            Restart
        }
    }
  }

  /**
    * For rec_insertif when cond = KEY_PRESENT
    */
  @inline private def attemptInsertIfKeyPresent(expected: MainNode[K, V],
                                                newMain: MainNode[K, V],
                                                ct: ConcurrentTrie[K, V],
                                                k: K, v: V, hc: Int, lev: Int): RequiredAction = {
    val backupGenerator = insertIfKeyIsPresentBackupGenerator(k, v, hc, lev)
    val gcasResult = GCAS_SYNC(expected, newMain, ct, backupGenerator)
    gcasResult match {
      case GcasSuccess => ReturnExpected
      case GcasGenFail => Restart
      case GcasCompareRecovery(_) =>
        getPreviousValue(gcasResult, k, hc, lev) match {
          case SomeValue(prevV) => Return(Some(prevV))
          case _ =>
            assert(assertion = false,
              "Key %s should have been present! actual result: %s".format(k, gcasResult))
            Restart
        }
      case GcasCompareFail(_) =>
        getPreviousValue(gcasResult, k, hc, lev) match {
          case NoValue => Return(None)
          case SubTree | TombNode => Retry
          case SomeValue(prevV) =>
            assert(assertion = false,
              "Key %s was present, and should have been replaced! actual result: %s".format(k, gcasResult))
            Restart
        }
    }
  }

  /**
    * For rec_insertif when cond = otherV
    */
  @inline private def attemptInsertIfValuePresent(expected: MainNode[K, V],
                                                  newMain: MainNode[K, V],
                                                  ct: ConcurrentTrie[K, V],
                                                  expectedV: V,
                                                  k: K, v: V, hc: Int, lev: Int): RequiredAction = {
    val backupGenerator = insertIfValueIsPresentBackupGenerator(expectedV, k, v, hc, lev)
    val gcasResult = GCAS_SYNC(expected, newMain, ct, backupGenerator)
    gcasResult match {
      case GcasSuccess | GcasCompareRecovery(_) => ReturnExpected
      case GcasGenFail => Restart
      case GcasCompareFail(_) =>
        getPreviousValue(gcasResult, k, hc, lev) match {
          case NoValue | SomeValue(_) => Return(None)
          case SubTree | TombNode => Retry
        }
    }
  }

  /** Inserts a new key value pair, given that a specific condition is met.
   *
   *  @param cond        null - don't care if the key was there; KEY_ABSENT - key wasn't there; KEY_PRESENT - key was there; other value `v` - key must be bound to `v`
   *  @return            null if unsuccessful, Option[V] otherwise (indicating previous value bound to the key)
   */
  @tailrec final def rec_insertif(k: K, v: V, hc: Int, cond: AnyRef, lev: Int, parent: INode[K, V], startgen: Gen, ct: ConcurrentTrie[K, V]): Option[V] = {
    val m = READ_MAIN()  // use -Yinline!

    m match {
      case cn: CNode[K, V] => // 1) a multiway node
        val (wasFound,flag,pos) = cn.findPositions(hc,lev)
        if (wasFound) {
          // 1a) insert below
          cn.array(pos) match {
            case in: INode[K, V] =>
              if (startgen eq in.gen)
                // Go down one level in the recursion
                /*return*/ in.rec_insertif(k, v, hc, cond, lev + 5, this, startgen, ct)
              else {
                attemptGenerationUpdate(cn, startgen, ct) match {
                  case Retry => /*retry*/ rec_insertif(k, v, hc, cond, lev, parent, startgen, ct)
                  case _ => /*restart*/ null
                }
              }
            case sn: SNode[K, V] => cond match {
              case null =>
                val (expectedRetVal, newMain) =
                  if (sn.hc == hc && sn.k == k) {
                    // Replacing existing value
                    (Some(sn.v), cn.updatedAt(pos, new SNode(k, v, hc), gen))
                  }
                  else {
                    // Hashcode collision
                    val rn = if (cn.gen eq gen) cn else cn.renewed(gen)
                    (None, rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen))
                  }
                  attemptInsert(cn, newMain, ct, k, v, hc, lev) match {
                    case ReturnExpected => expectedRetVal
                    case Return(r) => r.asInstanceOf[Option[V]]
                    case Restart => null
                    case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
                  }
              case INode.KEY_ABSENT =>
                if (sn.hc == hc && sn.k == k)
                  // Key already exists, return existing value without replacing
                  /*return*/ Some(sn.v)
                else {
                  // Hashcode collision
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen)
                  val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                  attemptInsertIfKeyAbsent(cn, nn, ct, k, v, hc, lev) match {
                    case ReturnExpected => None
                    case Return(r) => r.asInstanceOf[Option[V]]
                    case Restart => null
                    case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
                  }
                }
              case INode.KEY_PRESENT =>
                if (sn.hc != hc || sn.k != k) {
                  // No existing key present, so no need to replace
                  /*return*/ None
                }
                else {
                  val updatedCn = cn.updatedAt(pos, new SNode(k, v, hc), gen)
                  attemptInsertIfKeyPresent(cn, updatedCn, ct, k, v, hc, lev) match {
                    case ReturnExpected => Some(sn.v)
                    case Return(r) => r.asInstanceOf[Option[V]]
                    case Restart => null
                    case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
                  }
                }
              case otherV: V =>
                if (sn.hc == hc && sn.k == k && sn.v == otherV) {
                  val updatedCn = cn.updatedAt(pos, new SNode(k, v, hc), gen)
                  attemptInsertIfValuePresent(cn, updatedCn, ct, otherV, k, v, hc, lev) match {
                    case ReturnExpected => Some(otherV)
                    case Return(r) => r.asInstanceOf[Option[V]]
                    case Restart => null
                    case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
                  }
                }
                else
                  // Expected value or key not found
                  /*return*/ None
            }
          }
        }
        // If the key doesn't exist in the CNode
        else cond match {
          case null =>
            val rn = if (cn.gen eq gen) cn else cn.renewed(gen)
            val ncnode = rn.insertedAt(pos, flag, new SNode(k, v, hc), gen)
            attemptInsert(cn, ncnode, ct, k, v, hc, lev) match {
              case ReturnExpected => None
              case Return(r) => r.asInstanceOf[Option[V]]
              case Restart => null
              case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
            }
          case INode.KEY_ABSENT =>
            val rn = if (cn.gen eq gen) cn else cn.renewed(gen)
            val ncnode = rn.insertedAt(pos, flag, new SNode(k, v, hc), gen)
            attemptInsertIfKeyAbsent(cn, ncnode, ct, k, v, hc, lev) match {
              case ReturnExpected => None
              case Return(r) => r.asInstanceOf[Option[V]]
              case Restart => null
              case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
            }
          case INode.KEY_PRESENT => /*return*/ None
          case otherv: V => /*return*/ None
        }
      case tn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        /*restart*/ null
      case ln: LNode[K, V] =>
        cond match {
          case null =>
            val prevValue = ln.get(k)
            val newList = ln.inserted(k, v)
            attemptInsert(ln, newList, ct, k, v, hc, lev) match {
              case ReturnExpected => prevValue
              case Return(r) => r.asInstanceOf[Option[V]]
              case Restart => null
              case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
            }
          case INode.KEY_ABSENT =>
            ln.get(k) match {
              case optv @ Some(_) => /*return*/ optv
              case None =>
                val updatedLn = ln.inserted(k,v)
                attemptInsertIfKeyAbsent(ln, updatedLn, ct, k, v, hc, lev) match {
                  case ReturnExpected => None
                  case Return(r) => r.asInstanceOf[Option[V]]
                  case Restart => null
                  case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
                }
            }
          case INode.KEY_PRESENT =>
            ln.get(k) match {
              case None => /*return*/ None
              case existingValue @ Some(_) =>
                val updatedLn = ln.inserted(k,v)
                attemptInsertIfKeyPresent(ln, updatedLn, ct, k, v, hc, lev) match {
                  case ReturnExpected => existingValue
                  case Return(r) => r.asInstanceOf[Option[V]]
                  case Restart => null
                  case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
                }
            }
          case otherv: V =>
            ln.get(k) match {
              case s @ Some(v0) if v0 == otherv =>
                val updatedLn = ln.inserted(k,v)
                attemptInsertIfValuePresent(ln, updatedLn, ct, otherv, k, v, hc, lev) match {
                  case ReturnExpected => s
                  case Return(r) => r.asInstanceOf[Option[V]]
                  case Restart => null
                  case Retry => rec_insertif(k,v,hc,cond,lev,parent,startgen,ct)
                }
              case _ => /*return*/ None
            }
        }
    }
  }

  /**
    * @return The value matching k, or null if k is not in the map. Our implementation will never restart.
   */
  @tailrec final def rec_lookup(k: K, hc: Int, lev: Int, parent: INode[K, V]): AnyRef = {
    val m = READ_MAIN() // use -Yinline!

    m match {
      case cn: CNode[K, V] =>
        val (wasFound,flag,pos) = cn.findPositions(hc,lev)
        if (!wasFound) /*return*/ null
        else {
          val sub = cn.array(pos)
          sub match {
            case in: INode[K, V] =>
              // WARNING: There used to be a generation check here, and even a generation update if it failed.
              // I don't think readers actually have to do this, but maybe I missed something.
              // Removing this generation check can cause a reader to return a value that was already removed from the
              // CTrie, if a snapshot happens while the reader is traversing and then the CTrie gets mutated on a new Gen.
              // However, I still think this counts as linearizable - the reader will return a value that really was
              // valid at some point in its runtime.

              // We can always add something like this here to do the generation check:
              // if (!ct.isReadOnly && in.Gen != startgen)
              //    this.synchronized { renewGen(); }
              // retry current invocation

              // Go down one level in recursion
              /*return*/ in.rec_lookup(k, hc, lev + 5, this)
            case sn: SNode[K, V] =>
              if (sn.hc == hc && sn.k == k)
                // Found
                /*return*/ sn.v.asInstanceOf[AnyRef]
              else
                // Not found
                /*return*/ null
          }
        }
      case tn: TNode[K, V] =>
        // WARNING: The readers used to clean the TNode's parent here, but I removed it. I think it puts unnecessary
        // load on the readers, since the rec_remove method tries to recursively clean up the tree after creating a
        // TNode anyway. I don't think this causes a linearizability problem.
        if (tn.hc == hc && tn.k == k)
          /*return*/ tn.v.asInstanceOf[AnyRef]
        else /*return*/ null
      case ln: LNode[K, V] =>
        /*return*/ ln.get(k).asInstanceOf[Option[AnyRef]].orNull
    }
  }

  @inline private def removeBackupGenerator(k: K, v: V, hc: Int, lev: Int)
  : (MainNode[K, V]) => Option[MainNode[K, V]] = {
    case tn: TNode[K,V] => None
    case cn: CNode[K,V] =>
      val (wasFound,flag,pos) = cn.findPositions(hc, lev)
      if (!wasFound) None
      else {
        cn.array(pos) match {
          case in: INode[K,V] => /*return*/ None
          case sn: SNode[K,V] =>
            if (sn.hc == hc && sn.k == k && (v == null || sn.v == v))
            /*return*/ Option(cn.removedAt(pos, flag, gen))
            else /*return*/ None
        }
      }
    case ln: LNode[K,V] =>
      ln.get(k) match {
        case Some(prevV) if v == null || prevV == v => /*return*/ Option(ln.removed(k))
        case _ => /*return*/ None
      }
  }

  @inline private def attemptRemove(expected: MainNode[K, V],
                                    newMain: MainNode[K, V],
                                    ct: ConcurrentTrie[K, V],
                                    k: K, v: V, hc: Int, lev: Int): RequiredAction = {
    val gcasResult = GCAS_SYNC(expected, newMain, ct, removeBackupGenerator(k,v,hc,lev))
    gcasResult match {
      case GcasSuccess => ReturnExpected
      case GcasGenFail => Restart
      case GcasCompareFail(_) =>
        getPreviousValue(gcasResult, k, hc, lev) match {
          case NoValue =>  Return(None)
          case SubTree | TombNode => Retry
          case SomeValue(_) => Return(None) // Can happen if v != null && existing value != v
        }
      case GcasCompareRecovery(_) =>
        getPreviousValue(gcasResult, k, hc, lev) match {
          case SomeValue(prevV) => Return(Some(prevV))
          case _ =>
            assert(assertion = false,
              "Remove should not have recovered - previous value does not have key %s! %s".format(k, gcasResult))
            Restart
        }
    }
  }

  /** Removes the key associated with the given value.
   *
   *  @param v         if null, will remove the key irregardless of the value; otherwise removes only if binding contains that exact key and value
   *  @return          null if not successful, an Option[V] indicating the previous value otherwise
   */
  final def rec_remove(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: ConcurrentTrie[K, V]): Option[V] = {
    val m = READ_MAIN() // use -Yinline!

    // The following expression has no "return" commands, because the result is put into "res" instead of exiting
    // the whole method.
    val res = m match {
      case cn: CNode[K, V] =>
        val (wasFound,flag,pos) = cn.findPositions(hc,lev)
        if (!wasFound) /*return*/ None
        else {
          val sub = cn.array(pos)
          sub match {
            case in: INode[K, V] =>
              if (startgen eq in.gen)
                // Go down one level in recursion
                in.rec_remove(k, v, hc, lev + 5, this, startgen, ct)
              else {
                attemptGenerationUpdate(cn, startgen, ct) match {
                  case Retry => rec_remove(k, v, hc, lev, parent, startgen, ct)
                  case _ => /*restart*/ null
                }
              }
            case sn: SNode[K, V] =>
              if (sn.hc == hc && sn.k == k && (v == null || sn.v == v)) {
                val ncn = cn.removedAt(pos, flag, gen).toContracted(lev)
                attemptRemove(cn, ncn, ct, k, v, hc, lev) match {
                  case ReturnExpected => Some(sn.v)
                  case Return(optV) => optV.asInstanceOf[Option[V]]
                  case Retry => rec_remove(k, v, hc, lev, parent, startgen, ct)
                  case Restart => null
                }
              }
              // Key to remove was not found, return
              else None
          }
        }
      case tn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        // Retry from root
        null
      case ln: LNode[K, V] =>
        ln.get(k) match {
          case optV@Some(prevV) if v == null || prevV == v =>
            attemptRemove(ln, ln.removed(k), ct, k, v, hc, lev) match {
              case ReturnExpected => /*return*/ optV
              case Return(other) => /*return*/ other.asInstanceOf[Option[V]]
              case Retry => rec_remove(k, v, hc, lev, parent, startgen, ct)
              case Restart => null
            }
          // Key not found or value mismatch
          case _ => /*return*/ None
        }
    }

    // If removal did nothing (key was not found) or failed completely, then the tree is not changed and there's
    // no reason to cleanup. Also, if this INode is the root there's no parent to clean.
    if ((parent eq null) || res == null || res.isEmpty) /*return*/ res
    else {
      // The remove operation succeeded, we may need to clean (shrink) the subtree into the parent's CNode
      @tailrec def cleanParent(tn: TNode[K,V]) {
        // TODO: cleanParentBackupGenerator
        val pm = parent.READ_MAIN()
        pm match {
          case cn: CNode[K, V] =>
            val parentLevel = lev - 5
            val (wasFound,_,pos) = cn.findPositions(hc, parentLevel)
            if (!wasFound) {} // somebody already removed this i-node, we're done
            else {
              // Check that we are still children of the parent, otherwise there's no point cleaning
              val sub = cn.array(pos)
              if (sub eq this) {
                // Will shrink TNodes under cn into cn, and if cn contains only one SNode after the operation,
                // toContracted will return a TNode instead of a CNode. (which will cause the parent's parent shrink also)
                val ncn = cn.updatedAt(pos, tn.copyUntombed, gen).toContracted(lev - 5)
                if (!parent.GCAS_SYNC(cn, ncn, ct))
                // Retry cleanup, maybe the Gen changed or there was a concurrent modification
                  if (ct.READ_ROOT().gen == startgen) /*return*/ cleanParent(tn)
              }
            }
          case _ => /*return*/ // parent is no longer a cnode, we're done (some other thread already cleaned up)
        }
      }

      // Only clean if the removal we just performed caused us to become a TNode.
      READ_MAIN() match {
        case tn: TNode[K,V] => cleanParent(tn)
        case _ => //NOP
      }

      // After cleanup, we still need to return the actual value we removed.
      /*return*/ res
    }
  }

  /**
    * Note that this in fact cleans the parent, not *this*.
    * No backup generator used in GCAS_SYNC, since we don't care that much if the cleanup fails -
    * not worth holding the lock for it. (Also, toCompressed seems to be a very heavy operation)
    */
  private def clean(parent: INode[K, V], ct: ConcurrentTrie[K, V], lev: Int) {
    val m = parent.READ_MAIN()
    m match {
      // TODO: I don't understand why we give our parent's new CNode this.gen instead of parent.gen
      // This seems like a bug to me, but it also seems to not really matter - the CNode's gen doesn't mean much.
      case cn: CNode[K, V] => parent.GCAS_SYNC(cn, cn.toCompressed(lev, gen), ct)
      case _ =>
    }
  }

  /* this is a quiescent method! */
  def string(lev: Int) =
    "%sINode.%s -> %s".format("  " * lev, gen,
      READ_MAIN() match {
        case null => "<null>"
        case tn: TNode[_, _] => "TNode(%s, %s, %d, !)".format(tn.k, tn.v, tn.hc)
        case cn: CNode[_, _] => cn.string(lev)
        case ln: LNode[_, _] => ln.string(lev)
        case x => "<elem: %s>".format(x)
      })
}

object INode {
  val RESTART = new AnyRef
  val KEY_PRESENT = new AnyRef
  val KEY_ABSENT = new AnyRef

  def newRootNode[K, V] = {
    val gen = new Gen
    val cn = new CNode[K, V](0, new Array(0), gen)
    new INode[K, V](cn, gen)
  }
}

sealed trait PreviousValue

case class SomeValue[V](v: V) extends PreviousValue
case object NoValue extends PreviousValue
case object SubTree extends PreviousValue
case object TombNode extends PreviousValue


sealed trait RequiredAction

case class Return[T](t: T) extends RequiredAction
case object ReturnExpected extends RequiredAction
case object Retry extends RequiredAction
case object Restart extends RequiredAction