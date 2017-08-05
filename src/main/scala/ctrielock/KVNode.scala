package ctrielock

/**
  * <p>
  * Created by Tolstoyevsky on 05/08/2017.
  * </p>
  */
trait KVNode[K, V] {
  def kvPair: (K, V)
}
