package arcana.world

import scala.io.StdIn

object Repl {
  val version = "0.0"
  case class ArgumentFlags(typeInfo: Boolean)

  val fooEvaluat = (input: String) => println(input)
  val fooTypeEvaluat = (input: String) => println(input + ": type")
  def evaluatArguments(args: Array[String]): Unit = {
    val needTypeInfo = args.foldLeft(false)((flag, arg) => flag || arg.equals("--type"))
    ArgumentFlags(typeInfo) match {
      // TODO change fooEvaluat
      case ArgumentFlags(true) => startRepl(fooTypeEvaluat)
      case _                   => startRepl(fooEvaluat)
    }
  }

  def startRepl(evaluat: (String) => Unit) = {
    printf(
      "Welcome to Arcana %s \n" + "Type in expressions for evaluation.\n",
      this.version
    )
    var continue: Boolean = true;
    while (continue) {
      printf("Arcana> ")
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
