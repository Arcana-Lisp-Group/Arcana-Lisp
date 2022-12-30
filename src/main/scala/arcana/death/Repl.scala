package arcana.death

import scala.io.StdIn

class Repl(options: ReplOptions) {
  import Repl._
  printf(
    "\u001b[32;1m" +
      "Welcome to Arcana %s \n" +
      "Type in expressions for evaluation.\n" +
      "Type in exit or ctrl-d to quit\n" +
      "\u001b[0m",
    version
  )
  parse(true)(options)

}

object Repl {
  private val version = "0.0"

  private def parse(continue: Boolean)(implicit options: ReplOptions): Unit = {
    if (continue) {
      printf("\u001b[34;1mArcana> \u001b[0m")
      io.StdIn.readLine() match {
        case input if isExit(input) => println("quit"); parse(false)
        case input                  => evaluatCode(input, options); parse(true)
      }
    }
  }

  private def isExit(input: String): Boolean =
    if (input == null || input.equals("exit")) true else false

  private def evaluatCode(input: String, options: ReplOptions) = {
    println(input, options)
  }
}
