package test.scala.benchmarks

import org.scalameter.api._
import org.scalameter.picklers.noPickler._
import test.scala.benchmarks.Global._


object CTrieIterationBenchmark extends Bench.OfflineReport {
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

object CTrieLockIterationBenchmark extends Bench.OfflineReport {
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