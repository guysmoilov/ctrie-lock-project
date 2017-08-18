package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieMemoryOccupancyBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "CTrieMemory" in {
    measure method "AfterDelete" in {
      using(tries) in { ct =>
        val e = elems

        for (i <- 0 until sz) ct.update(e(i), e(i))
        for (i <- 0 until sz) ct.remove(e(i))

        Runtime.getRuntime.gc()
        while (true) {}
      }
    }
  }

  performance of "CTrieMemory" in {
    measure method "Memory" in {
      using(tries) in { ct =>
        val e = elems

        for (i <- 0 until sz) ct.update(e(i), e(i))

        Runtime.getRuntime.gc()
        while (true) {}
      }
    }
  }
}


object CTrieLockMemoryOccupancyBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "CTrie" in {
    measure method "AfterDelete" in {
      using(tries) in { ct =>
        val e = elems

        for (i <- 0 until sz) ct.update(e(i), e(i))
        for (i <- 0 until sz) ct.remove(e(i))

        Runtime.getRuntime.gc()
        while (true) {}
      }
    }
  }

  performance of "CTrie" in {
    measure method "Memory" in {
      using(tries) in { ct =>
        val e = elems

        for (i <- 0 until sz) ct.update(e(i), e(i))

        Runtime.getRuntime.gc()
        while (true) {}
      }
    }
  }
}

