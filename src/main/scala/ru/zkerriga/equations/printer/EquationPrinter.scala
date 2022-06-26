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

  private class Pretty[F[_]: Applicative] extends EquationPrinter[F]:
    override def print(equation: ZeroEquation): F[String] = Applicative[F].pure {
      printSummand(equation.summands.head, begin = true) +
        equation.summands.tail.foldLeft("") { (acc, summand) =>
          acc + " " + printSummand(summand)
        } + " = 0"
    }

    private def printSign(multiplierIsNegative: Boolean, begin: Boolean): String =
      if multiplierIsNegative && begin then "-"
      else if multiplierIsNegative then "- "
      else if begin then ""
      else "+ "

    private def printMultiplier(multiplier: Coefficient, shouldBePrinted: Boolean): String =
      if shouldBePrinted then multiplier.absoluteString else ""

    private def printVariable(variable: Variable, shouldBePrinted: Boolean): String =
      if shouldBePrinted then s"$variable"
      else ""

    private def printExponent(exponent: Coefficient, shouldBePrinted: Boolean): String =
      if shouldBePrinted then s"^$exponent" else ""

    private def printSummand(summand: Summand, begin: Boolean = false): String = {
      val variableShouldBePrinted =
        summand.multiplier != Coefficient.Zero && summand.exponent != Coefficient.Zero
      val multiplierShouldBePrinted =
        !variableShouldBePrinted || summand.multiplier != Coefficient.One && summand.multiplier != Coefficient.MinusOne
      val exponentShouldBePrinted = variableShouldBePrinted && summand.exponent != Coefficient.One

      printSign(summand.multiplier.isNegative, begin) +
        printMultiplier(summand.multiplier, multiplierShouldBePrinted) +
        printVariable(summand.variable, variableShouldBePrinted) +
        printExponent(summand.exponent, exponentShouldBePrinted)
    }

  end Pretty

  def default[F[_]: Applicative]: EquationPrinter[F] = new Impl[F]
  def pretty[F[_]: Applicative]: EquationPrinter[F]  = new Pretty[F]
