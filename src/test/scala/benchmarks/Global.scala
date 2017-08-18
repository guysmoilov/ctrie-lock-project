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
  val par: Option[Int] = Option(System.getProperty("par")).map(_.toInt)
  val lookups: Option[Int] = Option(System.getProperty("lookups")).map(_.toInt)
  val inserts: Option[Int] = Option(System.getProperty("inserts")).map(_.toInt)
  val removes: Option[Int] = Option(System.getProperty("removes")).map(_.toInt)
  val totalops: Option[Int] = Option(System.getProperty("totalops")).map(_.toInt)
  val lookupratio: Option[Int] = Option(System.getProperty("lookupratio")).map(_.toInt)
  val damping: Option[Double] = Option(System.getProperty("damping")).map(_.toDouble)
  val maxlinks: Option[Int] = Option(System.getProperty("maxlinks")).map(_.toInt)
  val pagegenerator = Option(System.getProperty("pagegenerator"))
  val updateFilled: Boolean = Option(System.getProperty("updateFilled")).map(_.toBoolean).getOrElse(false)
  val debug: Boolean = Option(System.getProperty("debug")).map(_.toBoolean).getOrElse(false)
  val elems: Array[Elem] = (for (i <- 0 until (sz * 2)) yield Elem(i)).toArray
}


