import scala.compat.Platform
import java.util.concurrent.ConcurrentHashMap
import Global._

object Main {
  var multiplier = 1
  def prefix: String = getClass().getName()

  def runBenchmark(noTimes: Int): List[Long] =
    for (i <- List.range(1, noTimes + 1)) yield {
      setUp
      val startTime = Platform.currentTime
      var i = 0; while (i < multiplier) {
        run()
        i += 1
      }
      val stopTime = Platform.currentTime
      //tearDown
      Platform.collectGarbage

      stopTime - startTime
    }

 def main(args: Array[String]) {
    if (args.length > 0) {
      val logFile = new java.io.OutputStreamWriter(System.out)
      if (args.length > 1) multiplier = args(1).toInt
      logFile.write(prefix)
      for (t <- runBenchmark(args(0).toInt))
        logFile.write("\t" + t)

      logFile.write(Platform.EOL)
      logFile.flush()
    } else {
      println("Usage: scala benchmarks.program <runs> ")
      println("   or: scala benchmarks.program <runs> <multiplier>")
      println("""
    The benchmark is run <runs> times, forcing a garbage collection between runs. The optional
    <multiplier> causes the benchmark to be repeated <multiplier> times, each time for <runs>
    executions.
      """)
    }
  }


  var chm = new ConcurrentHashMap[Elem, Elem]

  def setUp {
    chm = new ConcurrentHashMap[Elem, Elem]
    for (i <- 0 until sz) chm.put(elems(i), elems(i))
    Runtime.getRuntime.gc()
  }

  def run() {
    val p = par.get
    val step = sz / p

    val ins = for (i <- 0 until p) yield new Inserter(chm, i, step)

    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }

  class Inserter(chm: ConcurrentHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems

      while (i < until) {
        chm.put(e(i), e(i))
        i += 1
      }
    }
  }
}
