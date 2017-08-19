package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieMemoryOccupancyBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
  )

  performance of "MemoryOccupancy" in {
    measure method "AfterDelete" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test AfterDelete")
      } afterTests {
        if(debug) println("Finished AfterDelete")
      } in { _ =>
        val e = elems

        for (i <- 0 until sz) ct.update(e(i), e(i))
        for (i <- 0 until sz) ct.remove(e(i))

        Runtime.getRuntime.gc()
        while (true) {}
      }
    }

    measure method "Memory" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Memory")
      } afterTests {
        if(debug) println("Finished Memory")
      } in { _ =>
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

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
  )

  performance of "MemoryOccupancy" in {
    measure method "AfterDelete" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test AfterDelete")
      } afterTests {
        if(debug) println("Finished AfterDelete")
      } in { _ =>
        val e = elems

        for (i <- 0 until sz) ct.update(e(i), e(i))
        for (i <- 0 until sz) ct.remove(e(i))

        Runtime.getRuntime.gc()
        while (true) {}
      }
    }

    measure method "Memory" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Memory")
      } afterTests {
        if(debug) println("Finished Memory")
      } in { _ =>
        val e = elems

        for (i <- 0 until sz) ct.update(e(i), e(i))

        Runtime.getRuntime.gc()
        while (true) {}
      }
    }
  }
}

