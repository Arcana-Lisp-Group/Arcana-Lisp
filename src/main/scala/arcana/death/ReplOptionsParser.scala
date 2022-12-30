package arcana.death

case class ReplOptions(typeInfo: Boolean)

class ReplOptionsParser(args: Array[String]) {
  import ReplOptionsParser._
  val replOptions = args.foldLeft(ReplOptions(false))((options, argument) => {
    argument match {
      case "--help" => printHelpHint(); throw ReplException("");
      case "--type" => ReplOptions(true) // extend any other flag from options
      case other =>
        printHelpHint()
        throw ReplException(other + "is not an available option.\n");
    }
  })
  new Repl(replOptions)
}

object ReplOptionsParser {
  def printHelpHint() = {
    print(
      "\nUsage: arcana <options>\n\n" +
        "All options include:\n" +
        "   --help      get help info of Repl\n" +
        "   --type      open type info of Repl\n\n"
    )
  }
}
