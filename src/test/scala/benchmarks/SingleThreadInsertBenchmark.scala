package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieSingleThreadInsertBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
  )

  performance of "SingleThread" in {
    measure method "Insert" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Insert")
      } afterTests {
        if(debug) println("Finished Insert")
      } in { _ =>
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

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
  )

  performance of "SingleThread" in {
    measure method "Insert" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Insert")
      } afterTests {
        if(debug) println("Finished Insert")
      } in { _ =>
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

