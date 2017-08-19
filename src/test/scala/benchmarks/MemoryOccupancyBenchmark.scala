package test.scala.benchmarks

import org.scalameter.api._
import org.scalameter.picklers.noPickler._
import test.scala.benchmarks.Global._


object CTrieMemoryOccupancyBenchmark extends Bench.OfflineReport {
  import ctries2.ConcurrentTrie

  override def reporter: Reporter.Composite[Double] = Reporter.Composite(
    new RegressionReporter(
      RegressionReporter.Tester.Accepter(),
      RegressionReporter.Historian.Window(1)),
    HtmlReporter(embedDsv),
    new LoggingReporter()
  )
  override def executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.average,
    new Measurer.Default)
  override def persistor: Persistor.None.type = Persistor.None

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples,  // number of JVM instances
    exec.jvmflags -> jvmParams,                     // JVM params, used to limit reserved space (default: 2GB)
    reports.resultDir -> "target/benchmarks"
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


object CTrieLockMemoryOccupancyBenchmark extends Bench.OfflineReport {
  import ctrielock.ConcurrentTrie

  override def reporter: Reporter.Composite[Double] = Reporter.Composite(
    new RegressionReporter(
      RegressionReporter.Tester.Accepter(),
      RegressionReporter.Historian.Window(1)),
    HtmlReporter(embedDsv),
    new LoggingReporter()
  )
  override def executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.average,
    new Measurer.Default)
  override def persistor: Persistor.None.type = Persistor.None

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples,  // number of JVM instances
    exec.jvmflags -> jvmParams,                     // JVM params, used to limit reserved space (default: 2GB)
    reports.resultDir -> "target/benchmarks"
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

