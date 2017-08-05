package ctrielock

/**
  * <p>
  * Created by Tolstoyevsky on 05/08/2017.
  * </p>
  */
@Deprecated
final class FailedNode[K, V](p: MainNode[K, V]) extends MainNode[K, V] {
  WRITE_PREV(p)

  def string(lev: Int) = throw new UnsupportedOperationException

  override def toString = "FailedNode(%s)".format(p)
}
