package benchmarks

import benchmarks.Global._
import org.scalameter.api._
import org.scalameter.picklers.noPickler._


object CTrieMultiThreadSnapshotBenchmarks extends Bench.OfflineReport {
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

  performance of "CTrieSnapshot" in {
    measure method "RemoveMultipleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test RemoveMultipleWithSnapshot")
      } afterTests {
        if(debug) println("Finished RemoveMultipleWithSnapshot")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val snap = ct.snapshot()
        val ins = for (i <- 0 until par) yield new Remover(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }

    measure method "InsertMultipleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test RemoveMultipleWithSnapshot")
      } afterTests {
        if(debug) println("Finished RemoveMultipleWithSnapshot")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val snap = ct.snapshot()
        val ins = for (i <- 0 until par) yield new Updater(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }

    measure method "LookupMultipleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test LookupSingleWithSnapshot")
      } afterTests {
        if(debug) println("Finished LookupSingleWithSnapshot")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val snap = ct.snapshot()
        val ins = for (i <- 0 until par) yield new Looker(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }


  class Remover(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems

      while (i < until) {
        ct.remove(e(i))
        i += 1
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


object CTrieLockMultiThreadSnapshotBenchmarks extends Bench.OfflineReport {
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

  performance of "CTrieSnapshot" in {
    measure method "RemoveMultipleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test RemoveMultipleWithSnapshot")
      } afterTests {
        if(debug) println("Finished RemoveMultipleWithSnapshot")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val snap = ct.snapshot()
        val ins = for (i <- 0 until par) yield new Remover(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }

    measure method "InsertMultipleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test RemoveMultipleWithSnapshot")
      } afterTests {
        if(debug) println("Finished RemoveMultipleWithSnapshot")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val snap = ct.snapshot()
        val ins = for (i <- 0 until par) yield new Updater(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }

    measure method "LookupMultipleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test LookupSingleWithSnapshot")
      } afterTests {
        if(debug) println("Finished LookupSingleWithSnapshot")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val snap = ct.snapshot()
        val ins = for (i <- 0 until par) yield new Looker(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }


  class Remover(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems

      while (i < until) {
        ct.remove(e(i))
        i += 1
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


