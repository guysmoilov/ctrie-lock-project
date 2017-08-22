package benchmarks

case class Elem(i: Int) extends Comparable[Elem] {
  def compareTo(y: Elem): Int = i - y.i

  override def hashCode: Int = {
    // var hc = i * 0x9e3775cd
    // hc = java.lang.Integer.reverseBytes(hc)
    // hc * 0x9e3775cd
    
    var hc = i * 0x9e3775cd
    hc
    
    //var hc = i * 0x9e3775cd
    //hc + (hc << 16) + i
    
    //i
  }
}


object Global {
  // run example: bench -Dsz=500000 -Dpar=4 ctries.MultiInsertCtrie2 6 1
  val rep=100000
  val minWarmupRuns=1000
  val benchRuns=20
  val independentSamples=1
  val embedDsv=true
  val jvmParams: List[String] = Option(System.getProperty("jvmparams")).map(_.split(',').map(_.trim).toList).getOrElse(List("-Xmx1g", "-Xms1g", "-XX:CompileThreshold=100")) // -XX:InitialHeapSize=134217728 -XX:MaxHeapSize=2147483648 -XX:+PrintCommandLineFlags -XX:+UseCompressedClassPointers -XX:+UseCompressedOops -XX:+UseParallelGC
  val sz: Int = Option(System.getProperty("sz")).map(_.toInt).getOrElse(1000000)
  val par: Int = Option(System.getProperty("par")).map(_.toInt).getOrElse(1)
  val lookups: Int = Option(System.getProperty("lookups")).map(_.toInt).getOrElse(1000000)
  val inserts: Int = Option(System.getProperty("inserts")).map(_.toInt).getOrElse(1000000)
  val removes: Int = Option(System.getProperty("removes")).map(_.toInt).getOrElse(1000000)
  val totalops: Int = Option(System.getProperty("totalops")).map(_.toInt).getOrElse(4)
  val lookupratio: Int = Option(System.getProperty("lookupratio")).map(_.toInt).getOrElse(4)
  val damping: Double = Option(System.getProperty("damping")).map(_.toDouble).getOrElse(4)
  val maxlinks: Int = Option(System.getProperty("maxlinks")).map(_.toInt).getOrElse(4)
  val pagegenerator = Option(System.getProperty("pagegenerator"))
  val updateFilled: Boolean = Option(System.getProperty("updateFilled")).map(_.toBoolean).getOrElse(false)
  val debug: Boolean = Option(System.getProperty("debug")).map(_.toBoolean).getOrElse(true)
  val elems: Array[Elem] = (for (i <- 0 until (sz * 2)) yield Elem(i)).toArray

  println(s"Running test with $par threads for $rep times and $sz elemets")
}


