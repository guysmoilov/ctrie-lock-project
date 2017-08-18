package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieFixedLookupInsertsBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "FixedLookup" in {
    measure method "Update" in {
      using(tries) in { ct =>
        val step = sz / par
        val ins = for (i <- 0 until par) yield new Worker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  class Worker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      while (i < until) {
        // do an insert
        ct.update(e(i), e(i))
        i += 1

        // do some lookups
        var j = 0
        while (j < lookupratio) {
          ct.lookup(e(math.abs(j * 0x9e3775cd) % i))
          j += 1
        }
      }
    }
  }
}

object CTrieLockFixedLookupInsertsBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "FixedLookup" in {
    measure method "Update" in {
      using(tries) in { ct =>
        val step = sz / par
        val ins = for (i <- 0 until par) yield new Worker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  class Worker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      while (i < until) {
        // do an insert
        ct.update(e(i), e(i))
        i += 1

        // do some lookups
        var j = 0
        while (j < lookupratio) {
          ct.lookup(e(math.abs(j * 0x9e3775cd) % i))
          j += 1
        }
      }
    }
  }
}

