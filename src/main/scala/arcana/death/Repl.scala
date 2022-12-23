package arcana.world

import scala.io.StdIn

object Repl {
  val version = "0.0"

  def startRepl(evaluat: (String) => Unit) = {
    printf(
      "\u001b[32;1mWelcome to Arcana %s \n" + "Type in expressions for evaluation.\n\u001b[0m",
      this.version
    )
    var continue: Boolean = true;
    while (continue) {
      printf("\u001b[34;1mArcana> \u001b[0m")
      io.StdIn.readLine() match {
        case input if isExit(input) => println("quit"); continue = false
        case input                  => evaluat(input)
      }
    }
  }

  def isExit(input: String): Boolean = {
    input match {
      case null => true
      case _    => false
    }
  }
}
