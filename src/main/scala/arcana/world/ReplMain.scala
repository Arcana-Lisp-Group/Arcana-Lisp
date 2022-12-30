package arcana.world

import arcana.death.ReplException
import arcana.death.ReplOptionsParser

object ReplMain {
  def main(args: Array[String]): Unit = {
    try {
      new ReplOptionsParser(args)
    } catch {
      case e: ReplException => print(e.getMessage())
    }
  }
}
