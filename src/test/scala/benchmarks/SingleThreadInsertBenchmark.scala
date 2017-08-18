package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieSingleThreadInsertBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "SingleThread" in {
    measure method "Insert" in {
      using(tries) in { ct =>
        var i = 0
        val until = sz
        val e = elems
        while (i < until) {
          ct.update(e(i), e(i))
          i += 1
        }
      }
    }
  }
}

object CTrieLockSingleThreadInsertBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "SingleThread" in {
    measure method "Insert" in {
      using(tries) in { ct =>
        var i = 0
        val until = sz
        val e = elems
        while (i < until) {
          ct.update(e(i), e(i))
          i += 1
        }
      }
    }
  }
}

