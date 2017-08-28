package benchmarks

import benchmarks.Global._
import org.scalameter.api._
import org.scalameter.picklers.noPickler._

import scala.collection.immutable


object MultiThreadUpdateBenchmark extends Bench.OfflineReport {
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
  val howmany: Int = totalops / par

  var ct = new ctries2.ConcurrentTrie[Elem, Elem]
  var ctlock = new ctrielock.ConcurrentTrie[Elem, Elem]

  var ws: immutable.IndexedSeq[Worker] = for (i <- 0 until par) yield new Worker(ct, i, howmany)
  var wslock: immutable.IndexedSeq[WorkerLock] = for (i <- 0 until par) yield new WorkerLock(ctlock, i, howmany)

  var array: Array[Int] = Array.fill(lookups)(0) ++ Array.fill(inserts)(1) ++ Array.fill(removes)(2)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples,  // number of JVM instances
    exec.jvmflags -> jvmParams,                     // JVM params, used to limit reserved space (default: 2GB)
    reports.resultDir -> "target/benchmarks"
  )

  performance of "MultiThreadInsertUpdateRemove" in {
    measure method "Update" in {
      using(runs) setUp { _ =>
        ct = new ctries2.ConcurrentTrie[Elem, Elem]
        if (updateFilled) for (i <- 0 until sz) ct.put(elems(i), elems(i))
        ws = for (i <- 0 until par) yield new Worker(ct, i, howmany)
        array = Array.fill(lookups)(0) ++ Array.fill(inserts)(1) ++ Array.fill(removes)(2)
      } beforeTests {
        if (debug) println("Starting test Update")
      } afterTests {
        if (debug) println("Finished Update")
      } in { _ =>
        for (i <- ws) i.start()
        for (i <- ws) i.join()
      }
    }

    measure method "Update-Lock" in {
      using(runs) setUp { _ =>
        ctlock = new ctrielock.ConcurrentTrie[Elem, Elem]
        if (updateFilled) for (i <- 0 until sz) ctlock.put(elems(i), elems(i))
        wslock = for (i <- 0 until par) yield new WorkerLock(ctlock, i, howmany)
        array = Array.fill(lookups)(0) ++ Array.fill(inserts)(1) ++ Array.fill(removes)(2)
      } beforeTests {
        if (debug) println("Starting test Update-Lock")
      } afterTests {
        if (debug) println("Finished Update-Lock")
      } in { _ =>
        for (i <- wslock) i.start()
        for (i <- wslock) i.join()
      }
    }
  }

  class Worker(ct: ctries2.ConcurrentTrie[Elem, Elem], n: Int, howmany: Int) extends Thread {
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

  class WorkerLock(ct: ctrielock.ConcurrentTrie[Elem, Elem], n: Int, howmany: Int) extends Thread {
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


