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
  val rep = 1
  val sz: Int = System.getProperty("sz").toInt
  val par: Int = Option(System.getProperty("par")).map(_.toInt).get
  val lookups: Int = Option(System.getProperty("lookups")).map(_.toInt).get
  val inserts: Int = Option(System.getProperty("inserts")).map(_.toInt).get
  val removes: Int = Option(System.getProperty("removes")).map(_.toInt).get
  val totalops: Int = Option(System.getProperty("totalops")).map(_.toInt).get
  val lookupratio: Int = Option(System.getProperty("lookupratio")).map(_.toInt).get
  val damping: Double = Option(System.getProperty("damping")).map(_.toDouble).get
  val maxlinks: Int = Option(System.getProperty("maxlinks")).map(_.toInt).get
  val pagegenerator = Option(System.getProperty("pagegenerator"))
  val updateFilled: Boolean = Option(System.getProperty("updateFilled")).map(_.toBoolean).getOrElse(false)
  val debug: Boolean = Option(System.getProperty("debug")).map(_.toBoolean).getOrElse(false)
  val elems: Array[Elem] = (for (i <- 0 until (sz * 2)) yield Elem(i)).toArray
}


