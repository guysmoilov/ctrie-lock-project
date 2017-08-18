package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieMultiThreadInsertBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "MultiThread" in {
    measure method "Insert" in {
      using(tries) in { ct =>
        val p = par.get
        val step = sz / p

        val ins = for (i <- 0 until p) yield new Updater(ct, i, step)

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

object CTrieLockMultiThreadInsertBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "MultiThread" in {
    measure method "Insert" in {
      using(tries) in { ct =>
        val p = par.get
        val step = sz / p

        val ins = for (i <- 0 until p) yield new Updater(ct, i, step)

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

