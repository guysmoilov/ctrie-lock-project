package test.scala.benchmarks

import benchmarks.Global._
import org.scalameter.api._

import scala.collection.parallel.{IterableSplitter, _}



/*
object CTriePageRankBenchmark extends Bench.LocalTime {
  import ctries2.ConcurrentTrie

  val seeds: Gen[Int] = Gen.range("seed")(0, rep, 1)
  val tries: Gen[ConcurrentTrie[Int, Page]] = for (seed <- seeds) yield new ConcurrentTrie[Int, Page]

  val pages = Page.pages(sz)
  val prob1 = new Array[Double](sz)
  val prob2 = new Array[Double](sz)

  if (debug) println("Page set constructed.")

  performance of "PageRank" in {
    measure method "Bla" in {
      using(tries) in { ct =>
        val in = 1.0 / sz
        for (i <- 0 until sz) prob1(i) = in
        for (i <- 0 until sz) prob2(i) = in

        for (i <- 0 until sz) ct.put(i, pages(i))

        val epsilon = damping / sz / 1000
        var iter = 0
        while (ct.nonEmpty) {
          val (last, next) = if (iter % 2 == 0) (prob1, prob2) else (prob2, prob1)
          for ((name, page) <- ct) {
            var sum = 0.0
            for (pind <- page.incoming) sum += last(pind) / pages(pind).linksnum
            next(name) = (1 - damping) / sz + damping * sum
            if (next(name) - last(name) < epsilon) ct.remove(name)
          }
          iter += 1

          if (debug) if (iter % 1 == 0) println("Iteration %d, size %d".format(iter, ct.size))
        }

        if (debug) println("No. iterations: " + iter)
      }
    }
  }

  // a parallel ctrie created using the parallel collections framework
  class ParCtrie[K, V](szest: Int) extends mutable.ParMap[K, V] {
    val seq = new ConcurrentTrie[K, V]

    def clear() = throw new UnsupportedOperationException

    final def +=(kv: (K, V)) = {
      seq += kv
      this
    }

    final def remove(k: K): Option[V] = seq.remove(k)

    final def -=(k: K) = {
      seq -= k
      this
    }

    def splitter: IterableSplitter[(K, V)] =
      new CtrieSplitter(szest, seq.readOnlySnapshot().asInstanceOf[ConcurrentTrie[K, V]]) with SCPI

    def get(k: K): Option[V] = seq.get(k)

    def put(key: K, value: V): Option[V] = seq.put(key, value)

    def size = szest

    type SCPI = SignalContextPassingIterator[CtrieSplitter]

    class CtrieSplitter(szestimate: Int, ct: ctries2.ConcurrentTrie[K, V])
      extends ctries2.CtrieIterator[K, V](ct) with ParIterator {
      self: SCPI =>
      def remaining = szestimate // not using these ops
      def dup = throw new UnsupportedOperationException // not using views
      def split: Seq[CtrieSplitter] = subdivide.map { // probably won't use LNodes
        case ci: ctries2.CtrieIterator[K, V] =>
          val cs = new CtrieSplitter(szestimate / 2, ct) with SCPI
          cs.stack = ci.stack
          cs.stackpos = ci.stackpos
          cs.depth = ci.depth
          cs.current = ci.current
          cs
      }
    }
  }

  // the total number of links on the page
  // indices of the pages linked by this page
  // and the indices of the pages linking to this page
  case class Page(name: Int, linksnum: Int, var outgoing: Array[Int], var incoming: Array[Int]) {
    override def toString = "Page(%d, %d, %s, %s)".format(name, linksnum, outgoing.toList, incoming.toList)
  }

  object Page {
    val rand = new util.Random(324132L)

    def calcIncoming(pages: Array[Page]) {
      val sz = pages.length
      val incomings = new Array[collection.Set[Int]](sz)

      for (i <- 0 until sz) incomings(i) = collection.mutable.HashSet[Int]()
      for (i <- 0 until sz; linkto <- pages(i).outgoing) incomings(linkto) += i
      for (i <- 0 until sz) pages(i).incoming = incomings(i).toArray.filter(_ != i)
    }

    // creates a set of pages with most links to initial pages
    def initialGauss(sz: Int) = {
      val pages = (for (i <- 0 until sz) yield {
        val linknum = rand.nextInt(maxlinks)
        val outgoing = for (j <- 0 until linknum) yield {
          val lnk = (math.abs(rand.nextGaussian() / 4) * sz) max 0 min (sz - 1)
          lnk.toInt
        }
        Page(i, linknum, outgoing.toArray, null)
      }).toArray

      calcIncoming(pages)

      pages
    }

    def groups(sz: Int) = {
      val pages = (for (i <- 0 until sz) yield {
        val linknum = rand.nextInt(maxlinks)
        val outgoing = for (j <- 0 until linknum) yield {
          val lnk = (rand.nextGaussian() * maxlinks + i) max 0 min (sz - 1)
          lnk.toInt
        }
        Page(i, linknum, outgoing.toArray, null)
      }) toArray

      calcIncoming(pages)

      pages
    }

    def nonUniform(sz: Int) = {
      val pages = (for (i <- 0 until sz) yield {
        val linknum = rand.nextInt(maxlinks)
        val group = (i / maxlinks) * maxlinks
        val linksin = for (j <- 0 until linknum) yield {
          val lnk = (rand.nextGaussian() * maxlinks + group) max 0 min (sz - 1)
          lnk.toInt
        }
        val linksprev = for (j <- 0 until linknum) yield {
          val lnk = (rand.nextGaussian() * maxlinks + group - maxlinks) max 0 min (sz - 1)
          lnk.toInt
        }
        val linkshalf = for (j <- 0 until linknum) yield {
          val lnk = (rand.nextGaussian() * maxlinks + group / 2) max 0 min (sz - 1)
          lnk.toInt
        }
        Page(i, linknum, (linksin ++ linksprev ++ linkshalf).toArray, null)
      }) toArray

      calcIncoming(pages)

      pages
    }

    def pages(sz: Int) = pagegenerator match {
      case "gauss-init" => initialGauss(sz)
      case "groups" => groups(sz)
      case "non-uniform" => nonUniform(sz)
    }
  }

}
*/