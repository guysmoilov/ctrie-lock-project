package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieMultiThreadLookupBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
  )

  performance of "MultiThread" in {
    measure method "Lookup" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Lookup")
      } afterTests {
        if(debug) println("Finished Lookup")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val ins = for (i <- 0 until par) yield new Looker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  class Looker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems

      while (i < until) {
        ct.lookup(e(i))
        i += 1
      }
    }
  }
}

object CTrieLockMultiThreadLookupBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
  )

  performance of "MultiThread" in {
    measure method "Lookup" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Lookup")
      } afterTests {
        if(debug) println("Finished Lookup")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val ins = for (i <- 0 until par) yield new Looker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  class Looker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems

      while (i < until) {
        ct.lookup(e(i))
        i += 1
      }
    }
  }
}


