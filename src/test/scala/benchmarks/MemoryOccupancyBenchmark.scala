package benchmarks

import benchmarks.Global._
import org.scalameter.api._
import org.scalameter.picklers.noPickler._


object MemoryOccupancyBenchmark extends Bench.OfflineReport {
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
    new Measurer.MemoryFootprint)
  override def persistor: Persistor.None.type = Persistor.None
  override def measurer: Measurer.MemoryFootprint = new Executor.Measurer.MemoryFootprint

  var e: Array[Elem] = elems

  val runs: Gen[Int] = Gen.range("runs")(0, 0, 1)
  var ct: Gen[ctries2.ConcurrentTrie[Elem, Elem]] = for (_ <- runs) yield new ctries2.ConcurrentTrie[Elem, Elem]
  var ctlock: Gen[ctrielock.ConcurrentTrie[Elem, Elem]] = for (_ <- runs) yield new ctrielock.ConcurrentTrie[Elem, Elem]

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples,  // number of JVM instances
    exec.jvmflags -> jvmParams,                     // JVM params, used to limit reserved space (default: 2GB)
    reports.resultDir -> "target/benchmarks"
  )

  performance of "MemoryOccupancy" in {
    measure method "Memory" in {
      using(ct) setUp { _ =>
        e = elems
      } tearDown { _ =>
        Runtime.getRuntime.gc()
      } beforeTests {
        if(debug) println("Starting test Memory")
      } afterTests {
        if(debug) println("Finished Memory")
      } in { ct =>
        for (i <- 0 until sz) ct.update(e(i), e(i))
        ct // scalatest measures the memory footprint of the object the return value of the benchmark snippet references.
      }
    }

    measure method "Memory-Lock" in {
      using(ctlock) setUp { _ =>
        e = elems
      } tearDown { _ =>
        Runtime.getRuntime.gc()
      } beforeTests {
        if(debug) println("Starting test Memory")
      } afterTests {
        if(debug) println("Finished Memory")
      } in { ctlock =>
        for (i <- 0 until sz) ctlock.update(e(i), e(i))
        ctlock // scalatest measures the memory footprint of the object the return value of the benchmark snippet references.
      }
    }
  }
}

