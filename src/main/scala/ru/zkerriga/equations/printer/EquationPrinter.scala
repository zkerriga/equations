package ru.zkerriga.equations.printer

import ru.zkerriga.equations.parsing.models.{Summand, ZeroEquation}
import ru.zkerriga.equations.domain.{Coefficient, Variable}

object EquationPrinter {

  private def printMultiplier(multiplier: Coefficient, begin: Boolean): String =
    (if multiplier.isNegative && begin then "-"
     else if multiplier.isNegative then "- "
     else if begin then ""
     else "+ ") + multiplier.absoluteString

  private def printVariable(variable: Variable): String = s" * $variable"

  private def printExponent(exponent: Coefficient): String = s"^$exponent"

  private def printSummand(summand: Summand, begin: Boolean = false): String =
    printMultiplier(summand.multiplier, begin) +
      printVariable(summand.variable) +
      printExponent(summand.exponent)

  def printEquation(equation: ZeroEquation): String =
    printSummand(equation.summands.head, begin = true) +
      equation.summands.tail.foldLeft("") { (acc, summand) =>
        acc + " " + printSummand(summand)
      } + " = 0"
}
