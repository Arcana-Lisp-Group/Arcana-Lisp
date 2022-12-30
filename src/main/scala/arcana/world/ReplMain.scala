package arcana.world

object ReplMain {

  // this main will run a REPL
  def main(args: Array[String]): Unit = {
    try {
      new ReplOptionParser(args);
    } catch {
      case e: ReplException => {
        println(e.getMessage())
      }
    }
  }
}
