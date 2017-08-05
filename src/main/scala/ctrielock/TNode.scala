package ctrielock

/**
  * <p>
  * Created by Tolstoyevsky on 05/08/2017.
  * </p>
  */
final class TNode[K, V](final val k: K, final val v: V, final val hc: Int)
extends MainNode[K, V] with KVNode[K, V] {
  final def copy = new TNode(k, v, hc)
  final def copyTombed = new TNode(k, v, hc)
  final def copyUntombed = new SNode(k, v, hc)
  final def kvPair = (k, v)
  final def string(lev: Int) = ("  " * lev) + "TNode(%s, %s, %x, !)".format(k, v, hc)
}
