package arcana.world

object Main {

  // this main will run a REPL
  def main(args: Array[String]): Unit = {
    try {
      new ArgumentEvaluator().evaluat(args);
    } catch {
      case e: ArgumentException => {
        println(e.getMessage())
      }
    }
  }
}
