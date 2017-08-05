package ctrielock

import scala.collection.immutable.ListMap

/**
  * <p>
  * Created by Tolstoyevsky on 05/08/2017.
  * </p>
  */
final class LNode[K, V](final val listmap: ListMap[K, V])
extends MainNode[K, V] {
  def this(k: K, v: V) = this(ListMap(k -> v))
  def this(k1: K, v1: V, k2: K, v2: V) = this(ListMap(k1 -> v1, k2 -> v2))
  def inserted(k: K, v: V) = new LNode(listmap + ((k, v)))
  def removed(k: K) = {
    val updmap = listmap - k
    if (updmap.size > 1) new LNode(updmap)
    else {
      val (k, v) = updmap.iterator.next
      new TNode(k, v, ConcurrentTrie.computeHash(k)) // create it tombed so that it gets compressed on subsequent accesses
    }
  }
  def get(k: K) = listmap.get(k)
  def string(lev: Int) = (" " * lev) + "LNode(%s)".format(listmap.mkString(", "))
}
