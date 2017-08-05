package ctrielock

/**
  * <p>
  * Created by Tolstoyevsky on 05/08/2017.
  * </p>
  */
final class SNode[K, V](final val k: K, final val v: V, final val hc: Int)
extends BasicNode with KVNode[K, V] {
  final def copy = new SNode(k, v, hc)
  final def copyTombed = new TNode(k, v, hc)
  final def copyUntombed = new SNode(k, v, hc)
  final def kvPair = (k, v)
  final def string(lev: Int) = ("  " * lev) + "SNode(%s, %s, %x)".format(k, v, hc)
}
