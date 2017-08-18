package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieLookupInsertBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "LookupInsert" in {
    measure method "Regular" in {
      using(tries) in { ct =>
        val p = par.get
        val step = sz / p
        val ins = for (i <- 0 until p) yield new Worker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  class Worker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      val ratio = lookupratio.get
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      while (i < until) {
        // do an insert
        ct.update(e(i), e(i))
        i += 1

        // do some lookups
        var j = 0
        while (j < ratio) {
          ct.lookup(e(math.abs(j * 0x9e3775cd) % i))
          j += 1
        }
      }
    }
  }
}

object CTrieLockLookupInsertBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "LookupInsert" in {
    measure method "Regular" in {
      using(tries) in { ct =>
        val p = par.get
        val step = sz / p
        val ins = for (i <- 0 until p) yield new Worker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  class Worker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      val ratio = lookupratio.get
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      while (i < until) {
        // do an insert
        ct.update(e(i), e(i))
        i += 1

        // do some lookups
        var j = 0
        while (j < ratio) {
          ct.lookup(e(math.abs(j * 0x9e3775cd) % i))
          j += 1
        }
      }
    }
  }
}
