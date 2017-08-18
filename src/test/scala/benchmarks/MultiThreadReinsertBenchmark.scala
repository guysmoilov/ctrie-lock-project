package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieMultiThreadReinsertBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "MultiThread" in {
    measure method "Reinsert" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        Runtime.getRuntime.gc()

        val step = sz / par
        val ins = for (i <- 0 until par) yield new Updater(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  class Updater(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems

      while (i < until) {
        ct.update(e(i), e(i))
        i += 1
      }
    }
  }
}

object CTrieLockMultiThreadReinsertBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "MultiThread" in {
    measure method "Reinsert" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        Runtime.getRuntime.gc()

        val step = sz / par
        val ins = for (i <- 0 until par) yield new Updater(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  class Updater(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems

      while (i < until) {
        ct.update(e(i), e(i))
        i += 1
      }
    }
  }
}

