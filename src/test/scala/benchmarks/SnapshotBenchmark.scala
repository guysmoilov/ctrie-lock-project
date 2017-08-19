package test.scala.benchmarks


import org.scalameter.api._
import org.scalameter.picklers.noPickler._
import test.scala.benchmarks.Global._


object CTrieSnapshotBenchmark extends Bench.OfflineReport {
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
    measure method "RemoveSingle" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test RemoveSingle")
      } afterTests {
        if(debug) println("Finished RemoveSingle")
      } in { _ =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        for (i <- 1 until sz) ct.remove(elems(i))
      }
    }

    measure method "RemoveSingleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test RemoveSingleWithSnapshot")
      } afterTests {
        if(debug) println("Finished RemoveSingleWithSnapshot")
      } in { _ =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val snap = ct.snapshot()
        for (i <- 1 until sz) snap.remove(elems(i))
      }
    }

    measure method "InsertSingle" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test InsertSingle")
      } afterTests {
        if(debug) println("Finished InsertSingle")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
      }
    }

    measure method "InsertSingleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test InsertSingleWithSnapshot")
      } afterTests {
        if(debug) println("Finished InsertSingleWithSnapshot")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val snap = ct.snapshot()
        for (i <- 0 until sz) snap.update(elems(i), elems(i))
      }
    }

    measure method "RemoveMultiple" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) if(debug) println("Starting test RemoveMultiple")
      } afterTests {
        if(debug) println("Finished RemoveMultiple")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val ins = for (i <- 0 until par) yield new Remover(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }

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

    measure method "LookupSingle" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test LookupSingle")
      } afterTests {
        if(debug) println("Finished LookupSingle")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val ins = for (i <- 0 until par) yield new Looker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }

    measure method "LookupSingleWithSnapshot" in {
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
}


object CTrieLockSnapshotBenchmark extends Bench.OfflineReport {
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

  performance of "CTrieLockSnapshot" in {
    measure method "RemoveSingle" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test RemoveSingle")
      } afterTests {
        if(debug) println("Finished RemoveSingle")
      } in { _ =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        for (i <- 1 until sz) ct.remove(elems(i))
      }
    }

    measure method "RemoveSingleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test RemoveSingleWithSnapshot")
      } afterTests {
        if(debug) println("Finished RemoveSingleWithSnapshot")
      } in { _ =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val snap = ct.snapshot()
        for (i <- 1 until sz) snap.remove(elems(i))
      }
    }

    measure method "InsertSingle" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test InsertSingle")
      } afterTests {
        if(debug) println("Finished InsertSingle")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
      }
    }

    measure method "InsertSingleWithSnapshot" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test InsertSingleWithSnapshot")
      } afterTests {
        if(debug) println("Finished InsertSingleWithSnapshot")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val snap = ct.snapshot()
        for (i <- 0 until sz) snap.update(elems(i), elems(i))
      }
    }

    measure method "RemoveMultiple" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) if(debug) println("Starting test RemoveMultiple")
      } afterTests {
        if(debug) println("Finished RemoveMultiple")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val ins = for (i <- 0 until par) yield new Remover(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }

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

    measure method "LookupSingle" in {
      using(runs) setUp { _ =>
        ct = new ConcurrentTrie[Elem, Elem]
      } beforeTests {
        if(debug) println("Starting test LookupSingle")
      } afterTests {
        if(debug) println("Finished LookupSingle")
      } in { _ =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val step = sz / par
        val ins = for (i <- 0 until par) yield new Looker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }

    measure method "LookupSingleWithSnapshot" in {
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
}


