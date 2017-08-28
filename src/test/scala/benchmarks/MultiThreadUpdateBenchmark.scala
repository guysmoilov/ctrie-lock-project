package benchmarks

import benchmarks.Global._
import org.scalameter.api._
import org.scalameter.picklers.noPickler._


object CTrieMultiThreadUpdateBenchmark extends Bench.OfflineReport {
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
  val array: Array[Int] = Array.fill(lookups)(0) ++ Array.fill(inserts)(1) ++ Array.fill(removes)(2)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples,  // number of JVM instances
    exec.jvmflags -> jvmParams,                     // JVM params, used to limit reserved space (default: 2GB)
    reports.resultDir -> "target/benchmarks"
  )

  performance of "MultiThread" in {
    measure method "Update" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Update")
      } afterTests {
        if(debug) println("Finished Update")
      } in { _ =>
        if (updateFilled) for (i <- 0 until sz) ct.put(elems(i), elems(i))

        val howmany = totalops / par
        val ws = for (i <- 0 until par) yield new Worker(ct, i, howmany)

        for (i <- ws) i.start()
        for (i <- ws) i.join()
      }
    }
  }

  class Worker(ct: ConcurrentTrie[Elem, Elem], n: Int, howmany: Int) extends Thread {
    override def run() {
      var i = 0
      val until = howmany
      val e = elems
      val arrlen = array.length
      val s = sz

      while (i < until) {
        val imodsz = i % s
        array(i % arrlen) match {
          case 0 => ct.lookup(e(imodsz))
          case 1 => ct.update(e(imodsz), e(imodsz))
          case 2 => ct.remove(e(imodsz))
        }

        i += 1
      }
    }
  }
}


object CTrieLockMultiThreadUpdateBenchmark extends Bench.OfflineReport {
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
  val array: Array[Int] = Array.fill(lookups)(0) ++ Array.fill(inserts)(1) ++ Array.fill(removes)(2)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples,  // number of JVM instances
    exec.jvmflags -> jvmParams,                     // JVM params, used to limit reserved space (default: 2GB)
    reports.resultDir -> "target/benchmarks"
  )

  performance of "MultiThread" in {
    measure method "Update" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test Update")
      } afterTests {
        if(debug) println("Finished Update")
      } in { _ =>
        if (updateFilled) for (i <- 0 until sz) ct.put(elems(i), elems(i))

        val howmany = totalops / par
        val ws = for (i <- 0 until par) yield new Worker(ct, i, howmany)

        for (i <- ws) i.start()
        for (i <- ws) i.join()
      }
    }
  }

  class Worker(ct: ConcurrentTrie[Elem, Elem], n: Int, howmany: Int) extends Thread {
    override def run() {
      var i = 0
      val until = howmany
      val e = elems
      val arrlen = array.length
      val s = sz

      while (i < until) {
        val imodsz = i % s
        array(i % arrlen) match {
          case 0 => ct.lookup(e(imodsz))
          case 1 => ct.update(e(imodsz), e(imodsz))
          case 2 => ct.remove(e(imodsz))
        }

        i += 1
      }
    }
  }
}

