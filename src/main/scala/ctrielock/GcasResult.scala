package ctrielock

/**
  * <p>
  * Created by Tolstoyevsky on 10/08/2017.
  * </p>
  */
sealed trait GcasResult

case object GcasSuccess extends GcasResult
case object GcasGenFail extends GcasResult

sealed trait GcasCompareResult[K,V] extends GcasResult {
  val actualMain: MainNode[K,V]
}
case class GcasCompareRecovery[K,V](override val actualMain: MainNode[K,V]) extends GcasCompareResult[K,V]
case class GcasCompareFail[K,V](override val actualMain: MainNode[K,V]) extends GcasCompareResult[K,V]
