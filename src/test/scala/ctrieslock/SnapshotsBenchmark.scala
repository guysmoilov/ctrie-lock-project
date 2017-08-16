package ctrieslock

import org.scalameter.api._
import Global._


object CtrieBenchmark extends Bench.LocalTime {

  val seeds = Gen.range("seed")(0, 10, 1)
  val tries: Gen[ctries2.ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ctries2.ConcurrentTrie[Elem, Elem]

  performance of "SingleRemoving" in {
    measure method "Ctrie" in {
      using(tries) in { ct =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        for (i <- 1 until sz) ct.remove(elems(i))
      }
    }
  }
}
