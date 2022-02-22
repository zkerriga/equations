package ru.zkerriga.equations

import cats.Id
import ru.zkerriga.equations.parsing.EquationParser

object Main {
  def main(args: Array[String]): Unit = {
    val parser: EquationParser[Id] = EquationParser.make[Id]

    print("Write equation: ")
    val equation = scala.io.StdIn.readLine()

    println(parser.parse(equation))
  }
}
