package arcana.world

import scala.io.StdIn

case class ReplOptions(typeInfo: Boolean)

class Repl {
  import Repl._

  def start(options: ReplOptions) = {
    printf(
      "\u001b[32;1m" +
        "Welcome to Arcana %s \n" +
        "Type in expressions for evaluation.\n" +
        "Type in exit or ctrl-d to quit\n" +
        "\u001b[0m",
      version
    )
    var continue: Boolean = true;
    while (continue) {
      printf("\u001b[34;1mArcana> \u001b[0m")
      io.StdIn.readLine() match {
        case input if isExit(input) => println("quit"); continue = false
        case input                  => evaluatCode(input, options)
      }
    }
  }
}

object Repl {
  private val version = "0.0"

  private def isExit(input: String): Boolean = {
    if (input == null || input == "exit") {
      return true
    } else {
      return false
    }
  }
  private val evaluatCode = (input: String, options: ReplOptions) => {
    println(input, options)
  }
}
