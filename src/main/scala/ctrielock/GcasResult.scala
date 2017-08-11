package ctrielock

/**
  * <p>
  * Created by Tolstoyevsky on 10/08/2017.
  * </p>
  */
sealed trait GcasResult

case object GcasSuccess extends GcasResult
case class GcasCompareRecovery[K,V](replacedMain: MainNode[K,V]) extends GcasResult
case class GcasCompareFail[K,V](unexpectedMain: MainNode[K,V]) extends GcasResult
case object GcasGenFail extends GcasResult
