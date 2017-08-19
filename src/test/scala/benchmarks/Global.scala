package test.scala.benchmarks

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
  val rep=100
  val minWarmupRuns=500
  val benchRuns=20
  val independentSamples=1
  val embedDsv=false
  val jvmParams=List("-Xmx1024m", "-Xms1024m", "-XX:CompileThreshold=100")
  val sz=5000//: Int = System.getProperty("sz").toInt
  val par=4//: Int = Option(System.getProperty("par")).map(_.toInt).get
  val lookups=4 //: Int = Option(System.getProperty("lookups")).map(_.toInt).get
  val inserts=4 //: Int = Option(System.getProperty("inserts")).map(_.toInt).get
  val removes=4 //: Int = Option(System.getProperty("removes")).map(_.toInt).get
  val totalops=4 //: Int = Option(System.getProperty("totalops")).map(_.toInt).get
  val lookupratio=4 //: Int = Option(System.getProperty("lookupratio")).map(_.toInt).get
  val damping=4 //: Double = Option(System.getProperty("damping")).map(_.toDouble).get
  val maxlinks=4 //: Int = Option(System.getProperty("maxlinks")).map(_.toInt).get
  val pagegenerator=4 // = Option(System.getProperty("pagegenerator"))
  val updateFilled=false //: Boolean = Option(System.getProperty("updateFilled")).map(_.toBoolean).getOrElse(false)
  val debug=true //: Boolean = Option(System.getProperty("debug")).map(_.toBoolean).getOrElse(false)
  val elems: Array[Elem] = (for (i <- 0 until (sz * 2)) yield Elem(i)).toArray
}


