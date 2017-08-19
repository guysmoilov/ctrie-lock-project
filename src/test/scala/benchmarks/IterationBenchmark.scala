package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieIterationBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
  )

  performance of "CTrieIteration" in {
    measure method "Regular" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Regular")
      } afterTests {
        if(debug) println("Finished Regular")
      } in { _ =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val it = ct.iterator
        while (it.hasNext) it.next()
      }
    }

    measure method "ReadonlySnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test ReadonlySnapshot")
      } afterTests {
        if(debug) println("Finished ReadonlySnapshot")
      } in { _ =>
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

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
  )

  performance of "CTrieIteration" in {
    measure method "Regular" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Regular")
      } afterTests {
        if(debug) println("Finished Regular")
      } in { _ =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val it = ct.iterator
        while (it.hasNext) it.next()
      }
    }

    measure method "ReadonlySnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test ReadonlySnapshot")
      } afterTests {
        if(debug) println("Finished ReadonlySnapshot")
      } in { _ =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val ro = ct.readOnlySnapshot()
        val it = ro.iterator
        while (it.hasNext) it.next()
      }
    }
  }
}