package ctrielock

/**
  * <p>
  * Created by Tolstoyevsky on 10/08/2017.
  * </p>
  */
trait GcasResult

case object GcasSuccess extends GcasResult
abstract case class GcasCompareResult[K,V](actualMain: MainNode[K,V]) extends GcasResult
case class GcasCompareRecovery[K,V](replacedMain: MainNode[K,V]) extends GcasCompareResult[K,V](replacedMain)
case class GcasCompareFail[K,V](unexpectedMain: MainNode[K,V]) extends GcasCompareResult[K,V](unexpectedMain)
case object GcasGenFail extends GcasResult
