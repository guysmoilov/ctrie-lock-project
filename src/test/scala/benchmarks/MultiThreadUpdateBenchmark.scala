package test.scala.benchmarks

import org.scalameter.api._
import Global._


object CTrieMultiThreadUpdateBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  val array: Array[Int] = Array.fill(lookups)(0) ++ Array.fill(inserts)(1) ++ Array.fill(removes)(2)

  performance of "MultiThread" in {
    measure method "Update" in {
      using(tries) in { ct =>
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
      val arr = array
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

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  val array: Array[Int] = Array.fill(lookups)(0) ++ Array.fill(inserts)(1) ++ Array.fill(removes)(2)

  performance of "MultiThread" in {
    measure method "Update" in {
      using(tries) in { ct =>
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
      val arr = array
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

