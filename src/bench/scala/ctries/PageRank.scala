package ctries



import Global._
import scala.testing.Benchmark
import scala.collection.parallel._



class ParCtrie[K, V] extends mutable.ParMap[K, V] {
  val seq = new ctries2.ConcurrentTrie[K, V]
  
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
    new CtrieSplitter(seq.readOnlySnapshot().asInstanceOf[ctries2.ConcurrentTrie[K, V]]) with SCPI
  
  def get(k: K): Option[V] = seq.get(k)
  
  def put(key: K, value: V): Option[V] = seq.put(key, value)
  
  def size = splitter.size
  
  type SCPI = SignalContextPassingIterator[CtrieSplitter]
  
  class CtrieSplitter(ct: ctries2.ConcurrentTrie[K, V])
  extends ctries2.CtrieIterator[K, V](ct) with ParIterator {
  self: SCPI =>
    def remaining = throw new UnsupportedOperationException // not using these ops
    def dup = throw new UnsupportedOperationException // not using views
    def split: Seq[CtrieSplitter] = subdivide.map { // probably won't use LNodes
      case ci: ctries2.CtrieIterator[K, V] =>
        val cs = new CtrieSplitter(ct) with SCPI
        cs.stack = ci.stack
        cs.stackpos = ci.stackpos
        cs.depth = ci.depth
        cs.current = ci.current
        cs
    }
  }
  
}



object PageRank extends Benchmark {
  def run() {
    val pct = new ParCtrie[Int, Int]
    for (i <- 0 until 100) pct.put(i, i)
    
    for (kv <- pct) println(kv)
  }
}