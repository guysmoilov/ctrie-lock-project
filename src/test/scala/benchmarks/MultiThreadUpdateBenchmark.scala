package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieMultiThreadUpdateBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)
  val array: Array[Int] = Array.fill(lookups)(0) ++ Array.fill(inserts)(1) ++ Array.fill(removes)(2)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
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

object CTrieLockMultiThreadUpdateBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  var ct = new ConcurrentTrie[Elem, Elem]
  val runs: Gen[Int] = Gen.single("run")(rep)
  val array: Array[Int] = Array.fill(lookups)(0) ++ Array.fill(inserts)(1) ++ Array.fill(removes)(2)

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> minWarmupRuns,            // minimum num of warmups
    exec.benchRuns -> benchRuns,                    // desired num of measurements
    exec.independentSamples -> independentSamples   // number of JVM instances
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

