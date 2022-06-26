package ru.zkerriga.equations.printer

import cats.Applicative
import ru.zkerriga.equations.parsing.models.{Summand, ZeroEquation}
import ru.zkerriga.equations.domain.{Coefficient, Variable}

trait EquationPrinter[F[_]]:
  def print(equation: ZeroEquation): F[String]

object EquationPrinter:
  private final class Impl[F[_]: Applicative] extends EquationPrinter[F]:
    override def print(equation: ZeroEquation): F[String] = Applicative[F].pure {
      printSummand(equation.summands.head, begin = true) +
        equation.summands.tail.foldLeft("") { (acc, summand) =>
          acc + " " + printSummand(summand)
        } + " = 0"
    }

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

  end Impl

  def make[F[_]: Applicative]: EquationPrinter[F] = new Impl[F]
