package ctrielock

/**
  * <p>
  * Created by Tolstoyevsky on 10/08/2017.
  * </p>
  */
trait GcasResult

case object GcasSuccess extends GcasResult
case class GcasCompareRecovery[K,V](actualMain: MainNode[K,V]) extends GcasResult
case class GcasCompareFail[K,V](actualMain: MainNode[K,V]) extends GcasResult
case object GcasGenFail extends GcasResult
