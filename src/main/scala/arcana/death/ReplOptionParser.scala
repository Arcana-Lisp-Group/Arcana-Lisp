package arcana.world

class ReplOptionParser(args: Array[String]) {
  import ReplOptionParser._

  val replOptions = args.foldLeft(ReplOptions(false))((options, argument) => {
    if (availableOptions.contains(argument)) {
      argument match {
        case "--help" => printHelpHint(); throw ReplException("") // help break
        case "--type" =>
          ReplOptions(true) // extend any other argument from options
      }
    } else {
      throw ReplException(
        argument + " is not an available option.\n" +
          "type arcana --help for more infomation.\n"
      )
    }
  })

  new Repl(replOptions)
}

object ReplOptionParser {
  private val availableOptions = List[String]("--help", "--type")
  private def printHelpHint() = {
    print(
      "\nUsage: arcana <options>\n\n" +
        "All options include:\n" +
        "   --help          get help info of REPL\n" +
        "   --type          open type info in REPL\n"
    )
  }
}
