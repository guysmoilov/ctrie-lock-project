package benchmarks

import benchmarks.Global._
import org.scalameter.api._
import org.scalameter.picklers.noPickler._

import scala.collection.immutable


object MultiThreadBenchmarks extends Bench.OfflineReport {
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

  val runs: Gen[Int] = Gen.single("run")(rep)
  val step: Int = sz / par

  var ct = new ctries2.ConcurrentTrie[Elem, Elem]
  var ctlock = new ctrielock.ConcurrentTrie[Elem, Elem]

  var ins_insert: immutable.IndexedSeq[Updater] = for (i <- 0 until par) yield new Updater(ct, i, step)
  var inslock_insert: immutable.IndexedSeq[UpdaterLock] = for (i <- 0 until par) yield new UpdaterLock(ctlock, i, step)

  var ins_lookup: immutable.IndexedSeq[Looker] = for (i <- 0 until par) yield new Looker(ct, i, step)
  var inslock_lookup: immutable.IndexedSeq[LookerLock] = for (i <- 0 until par) yield new LookerLock(ctlock, i, step)

  var ins_remove: immutable.IndexedSeq[Remover] = for (i <- 0 until par) yield new Remover(ct, i, step)
  var inslock_remove: immutable.IndexedSeq[RemoverLock] = for (i <- 0 until par) yield new RemoverLock(ctlock, i, step)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns, // minimum num of warmups
    exec.benchRuns -> benchRuns, // desired num of measurements
    exec.independentSamples -> independentSamples, // number of JVM instances
    exec.jvmflags -> jvmParams, // JVM params, used to limit reserved space (default: 2GB)
    reports.resultDir -> "target/benchmarks"
  )

  performance of "MultiThreadCtrie" in {

    /* Multi thread insert benchmarks */
    measure method "Insert" in {
      using(runs) setUp { _ =>
        ct = new ctries2.ConcurrentTrie[Elem, Elem]
        ins_insert = for (i <- 0 until par) yield new Updater(ct, i, step)
      } beforeTests {
        if (debug) println("Starting test Insert")
      } afterTests {
        if (debug) println("Finished Insert")
      } in { _ =>
        for (i <- ins_insert) i.start()
        for (i <- ins_insert) i.join()
      }
    }

    measure method "Insert-Lock" in {
      using(runs) setUp { _ =>
        ctlock = new ctrielock.ConcurrentTrie[Elem, Elem]
        inslock_insert = for (i <- 0 until par) yield new UpdaterLock(ctlock, i, step)
      } beforeTests {
        if (debug) println("Starting test Insert-Lock")
      } afterTests {
        if (debug) println("Finished Insert-Lock")
      } in { _ =>
        for (i <- inslock_insert) i.start()
        for (i <- inslock_insert) i.join()
      }
    }

    /* Multi thread lookup benchmarks */
    measure method "Lookup" in {
      using(runs) setUp { _ =>
        ct = new ctries2.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        ins_lookup = for (i <- 0 until par) yield new Looker(ct, i, step)
      } beforeTests {
        if (debug) println("Starting test Lookup")
      } afterTests {
        if (debug) println("Finished Lookup")
      } in { _ =>
        for (i <- ins_lookup) i.start()
        for (i <- ins_lookup) i.join()
      }
    }

    measure method "Lookup-Lock" in {
      using(runs) setUp { _ =>
        ctlock = new ctrielock.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ctlock.update(elems(i), elems(i))
        inslock_lookup = for (i <- 0 until par) yield new LookerLock(ctlock, i, step)
      } beforeTests {
        if (debug) println("Starting test Lookup-Lock")
      } afterTests {
        if (debug) println("Finished Lookup-Lock")
      } in { _ =>
        for (i <- inslock_lookup) i.start()
        for (i <- inslock_lookup) i.join()
      }
    }

    /* Multi thread remove benchmarks */
    measure method "Remove" in {
      using(runs) setUp { _ =>
        ct = new ctries2.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        ins_remove = for (i <- 0 until par) yield new Remover(ct, i, step)
      } beforeTests {
        if (debug) println("Starting test Remove")
      } afterTests {
        if (debug) println("Finished Remove")
      } in { _ =>
        for (i <- ins_remove) i.start()
        for (i <- ins_remove) i.join()
      }
    }

    measure method "Remove-Lock" in {
      using(runs) setUp { _ =>
        ctlock = new ctrielock.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ctlock.update(elems(i), elems(i))
        inslock_remove = for (i <- 0 until par) yield new RemoverLock(ctlock, i, step)
      } beforeTests {
        if (debug) println("Starting test Remove-Lock")
      } afterTests {
        if (debug) println("Finished Remove-Lock")
      } in { _ =>
        for (i <- inslock_remove) i.start()
        for (i <- inslock_remove) i.join()
      }
    }
  }


  performance of "MultiThreadSnapshot" in {

    /* Multi thread snapshot insert benchmarks */
    measure method "Insert" in {
      using(runs) setUp { _ =>
        ct = new ctries2.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val snap = ct.snapshot()
        ins_insert = for (i <- 0 until par) yield new Updater(snap, i, step)
      } beforeTests {
        if (debug) println("Starting test Insert-Snapshot")
      } afterTests {
        if (debug) println("Finished Insert-Snapshot")
      } in { _ =>
        for (i <- ins_insert) i.start()
        for (i <- ins_insert) i.join()
      }
    }

    measure method "Insert-Lock" in {
      using(runs) setUp { _ =>
        ctlock = new ctrielock.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ctlock.update(elems(i), elems(i))
        val snap = ctlock.snapshot()
        inslock_insert = for (i <- 0 until par) yield new UpdaterLock(snap, i, step)
      } beforeTests {
        if (debug) println("Starting test Insert-Snapshot-Lock")
      } afterTests {
        if (debug) println("Finished Insert-Snapshot-Lock")
      } in { _ =>
        for (i <- inslock_insert) i.start()
        for (i <- inslock_insert) i.join()
      }
    }

    measure method "Lookup" in {
      using(runs) setUp { _ =>
        ct = new ctries2.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val snap = ct.snapshot()
        ins_lookup = for (i <- 0 until par) yield new Looker(snap, i, step)
      } beforeTests {
        if (debug) println("Starting test Lookup-Snapshot")
      } afterTests {
        if (debug) println("Finished Lookup-Snapshot")
      } in { _ =>
        for (i <- ins_lookup) i.start()
        for (i <- ins_lookup) i.join()
      }
    }

    measure method "Lookup-Lock" in {
      using(runs) setUp { _ =>
        ctlock = new ctrielock.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ctlock.update(elems(i), elems(i))
        val snap = ctlock.snapshot()
        inslock_lookup = for (i <- 0 until par) yield new LookerLock(snap, i, step)
      } beforeTests {
        if (debug) println("Starting test Lookup-Snapshot-Lock")
      } afterTests {
        if (debug) println("Finished Lookup-Snapshot-Lock")
      } in { _ =>
        for (i <- inslock_lookup) i.start()
        for (i <- inslock_lookup) i.join()
      }
    }

    measure method "Remove" in {
      using(runs) setUp { _ =>
        ct = new ctries2.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val snap = ct.snapshot()
        ins_remove = for (i <- 0 until par) yield new Remover(snap, i, step)
      } beforeTests {
        if (debug) println("Starting test Remove-Snapshot")
      } afterTests {
        if (debug) println("Finished Remove-Snapshot")
      } in { _ =>
        for (i <- ins_remove) i.start()
        for (i <- ins_remove) i.join()
      }
    }

    measure method "Remove-Lock" in {
      using(runs) setUp { _ =>
        ctlock = new ctrielock.ConcurrentTrie[Elem, Elem]
        for (i <- 0 until sz) ctlock.update(elems(i), elems(i))
        val snap = ctlock.snapshot()
        inslock_remove = for (i <- 0 until par) yield new RemoverLock(snap, i, step)
      } beforeTests {
        if (debug) println("Starting test Remove-Snapshot-Lock")
      } afterTests {
        if (debug) println("Finished Remove-Snapshot-Lock")
      } in { _ =>
        for (i <- inslock_remove) i.start()
        for (i <- inslock_remove) i.join()
      }
    }
  }

  class Updater(ct: ctries2.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
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
  class UpdaterLock(ct: ctrielock.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
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
  class Looker(ct: ctries2.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
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
  class LookerLock(ct: ctrielock.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
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
  class Remover(ct: ctries2.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
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
  class RemoverLock(ct: ctrielock.ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
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

}

