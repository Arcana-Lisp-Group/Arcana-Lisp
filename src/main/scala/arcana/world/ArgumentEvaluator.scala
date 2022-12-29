package arcana.world

import scala.util.Try

class ArgumentEvaluator {
  def evaluat(args: Array[String]): Unit = {
    import ArgumentEvaluator._

    var needTypeInfo: Boolean = false // TODO add more judge flag
    args.foreach((arg: String) => {
      if (availableOptions.contains(arg)) {
        arg match {
          case "--help" => {
            printHelpHint
            return
          }
          case "--type" => needTypeInfo = true // TODO add more options
        }
      } else {
        throw ArgumentException(
          arg + " is not an available option.\n" +
            "type arcana --help for more infomation.\n"
        )
      }
    })

    new Repl().start(new ReplOptions(needTypeInfo))
  }
}

object ArgumentEvaluator {
  private val availableOptions = List[String]("--help", "--type")
  private def printHelpHint = {
    print(
      "\nUsage: arcana <options>\n\n" +
        "All options include:\n" +
        "   --help          get help info of REPL\n" +
        "   --type        open type info in REPL\n"
    )
  }
}
