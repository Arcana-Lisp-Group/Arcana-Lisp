package arcana.world

object Main {
  case class ArgumentFlags(typeInfo: Boolean)
  def evaluatArguments(args: Array[String]): ArgumentFlags = {
    val needTypeInfo =
      args.foldLeft(false)((flag, arg) => flag || arg.equals("--type"))
    ArgumentFlags(needTypeInfo)
  }

  def main(args: Array[String]): Unit = {
    val fooEvaluat = (input: String) => println(input)
    val fooTypeEvaluat = (input: String) => println(input + ": type")
    evaluatArguments(args) match {
      case ArgumentFlags(true) => Repl.startRepl(fooTypeEvaluat)
      case _                   => Repl.startRepl(fooEvaluat)
    }
  }
}
