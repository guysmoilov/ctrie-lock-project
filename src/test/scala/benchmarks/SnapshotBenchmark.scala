package test.scala.benchmarks


import org.scalameter.api._
import test.scala.benchmarks.Global._


object CTrieSnapshotBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "CTrieSnapshot" in {
    measure method "RemoveSingle" in {
      using(tries) in { ct =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        for (i <- 1 until sz) ct.remove(elems(i))
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "RemoveSingleWithSnapshot" in {
      using(tries) in { ct =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val snap = ct.snapshot()
        for (i <- 1 until sz) snap.remove(elems(i))
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "InsertSingle" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "InsertSingleWithSnapshot" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val snap = ct.snapshot()
        for (i <- 0 until sz) snap.update(elems(i), elems(i))
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

  performance of "CTrieSnapshot" in {
    measure method "RemoveMultiple" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val p = par.get
        val step = sz / p

        val ins = for (i <- 0 until p) yield new Remover(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "RemoveMultipleWithSnapshot" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val p = par.get
        val step = sz / p
        val snap = ct.snapshot()

        val ins = for (i <- 0 until p) yield new Remover(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
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

  performance of "CTrieSnapshot" in {
    measure method "LookupSingle" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val p = par.get
        val step = sz / p

        val ins = for (i <- 0 until p) yield new Looker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "LookupSingleWithSnapshot" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val p = par.get
        val step = sz / p
        val snap = ct.snapshot()

        val ins = for (i <- 0 until p) yield new Looker(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }
}

object CTrieLockSnapshotBenchmark extends Bench.LocalTime {
  import ctrielock.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Elem, Elem]] = for (seed <- seeds) yield new ConcurrentTrie[Elem, Elem]

  performance of "CTrieSnapshot" in {
    measure method "RemoveSingle" in {
      using(tries) in { ct =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        for (i <- 1 until sz) ct.remove(elems(i))
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "RemoveSingleWithSnapshot" in {
      using(tries) in { ct =>
        for (i <- 1 until sz) ct.put(elems(i), elems(i))
        val snap = ct.snapshot()
        for (i <- 1 until sz) snap.remove(elems(i))
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "InsertSingle" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "InsertSingleWithSnapshot" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val snap = ct.snapshot()
        for (i <- 0 until sz) snap.update(elems(i), elems(i))
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

  performance of "CTrieSnapshot" in {
    measure method "RemoveMultiple" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val p = par.get
        val step = sz / p

        val ins = for (i <- 0 until p) yield new Remover(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "RemoveMultipleWithSnapshot" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val p = par.get
        val step = sz / p
        val snap = ct.snapshot()

        val ins = for (i <- 0 until p) yield new Remover(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
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

  performance of "CTrieSnapshot" in {
    measure method "LookupSingle" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val p = par.get
        val step = sz / p

        val ins = for (i <- 0 until p) yield new Looker(ct, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }

  performance of "CTrieSnapshot" in {
    measure method "LookupSingleWithSnapshot" in {
      using(tries) in { ct =>
        for (i <- 0 until sz) ct.update(elems(i), elems(i))
        val p = par.get
        val step = sz / p
        val snap = ct.snapshot()

        val ins = for (i <- 0 until p) yield new Looker(snap, i, step)

        for (i <- ins) i.start()
        for (i <- ins) i.join()
      }
    }
  }
}

