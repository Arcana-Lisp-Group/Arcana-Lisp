package arcana.empress

import org.scalatest.funsuite.AnyFunSuite
import java.io.{BufferedReader, FileReader, BufferedWriter, FileWriter}
import arcana.empress.{Lexer, TokenPrettyPrinter, LexerOptions}

class LexerTest extends AnyFunSuite {
  import LexerTest._

  testList.foreach((name) => test(name) {
    val reader = new BufferedReader(new FileReader(s"src/test/arc/$name"))
    val code = readFromFile(reader)

    val writer = new BufferedWriter(new FileWriter(s"src/test/tokens/$name.tokens"))
    writer.write(TokenPrettyPrinter(Lexer(code, LexerOptions(None, true)).tokens))
    writer.close()
  })
}

object LexerTest {
  private val testList = List(
    "hello.arc", "basic.arc"
  )

  private def readFromFile(reader: BufferedReader, prev: String = ""): String = {
    val line = reader.readLine()
    if (line == null) prev
    else readFromFile(reader, s"$prev$line\n")
  }
}
