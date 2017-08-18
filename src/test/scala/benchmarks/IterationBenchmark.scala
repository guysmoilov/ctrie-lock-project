package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieIterationBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "CTrieIteration" in {
    measure method "Regular" in {
      using(tries) in { ct =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val it = ct.iterator
        while (it.hasNext) it.next()
      }
    }
  }

  performance of "CTrieIteration" in {
    measure method "ReadonlySnapshot" in {
      using(tries) in { ct =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val ro = ct.readOnlySnapshot()
        val it = ro.iterator
        while (it.hasNext) it.next()
      }
    }
  }
}

object CTrieLockIterationBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "CTrieIteration" in {
    measure method "Regular" in {
      using(tries) in { ct =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val it = ct.iterator
        while (it.hasNext) it.next()
      }
    }
  }

  performance of "CTrieIteration" in {
    measure method "ReadonlySnapshot" in {
      using(tries) in { ct =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val ro = ct.readOnlySnapshot()
        val it = ro.iterator
        while (it.hasNext) it.next()
      }
    }
  }
}