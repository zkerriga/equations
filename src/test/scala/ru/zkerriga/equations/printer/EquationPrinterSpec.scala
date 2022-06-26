package ru.zkerriga.equations.printer

import cats.Id
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.zkerriga.equations.domain.{Coefficient, Variable}
import ru.zkerriga.equations.parsing.core.{EquationSides, ParsingResult, PreParsers}
import ru.zkerriga.equations.parsing.models.{Summand, ZeroEquation}

class EquationPrinterSpec extends AnyFlatSpec with Matchers:
  "printEquation" should "correctly print begin" in {
    printingDefault(
      Summand(Coefficient(-12), Variable("x"), Coefficient(-9))
    ) shouldBe "-12 * X^-9 = 0"

    printingDefault(
      Summand(Coefficient(12), Variable("x"), Coefficient(9))
    ) shouldBe "12 * X^9 = 0"

    printingDefault(
      Summand(Coefficient(1), Variable("x"), Coefficient(0))
    ) shouldBe "1 * X^0 = 0"
  }

  it should "correctly print long summands" in {
    printingDefault(
      Summand(Coefficient(1), Variable("x"), Coefficient(0)),
      Summand(Coefficient(-30), Variable("y"), Coefficient(20)),
      Summand(Coefficient(1), Variable("x"), Coefficient(0)),
    ) shouldBe "1 * X^0 - 30 * Y^20 + 1 * X^0 = 0"

    printingDefault(
      Summand(Coefficient(-2), Variable("Z"), Coefficient(1)),
      Summand(Coefficient(30), Variable("y"), Coefficient(0)),
      Summand(Coefficient(-100), Variable("Yeah"), Coefficient(-13001)),
    ) shouldBe "-2 * Z^1 + 30 * Y^0 - 100 * YEAH^-13001 = 0"
  }

  "printEquation in pretty" should "correctly print begin" in {
    printingPretty(
      Summand(Coefficient(-12), Variable("x"), Coefficient(-9))
    ) shouldBe "-12X^-9 = 0"

    printingPretty(
      Summand(Coefficient(12), Variable("x"), Coefficient(9))
    ) shouldBe "12X^9 = 0"

    printingPretty(
      Summand(Coefficient(1), Variable("x"), Coefficient(0))
    ) shouldBe "1 = 0"
  }

  it should "correctly print long summands" in {
    printingPretty(
      Summand(Coefficient(1), Variable("x"), Coefficient(0)),
      Summand(Coefficient(-30), Variable("y"), Coefficient(20)),
      Summand(Coefficient(1), Variable("x"), Coefficient(0)),
    ) shouldBe "1 - 30Y^20 + 1 = 0"

    printingPretty(
      Summand(Coefficient(-2), Variable("Z"), Coefficient(1)),
      Summand(Coefficient(30), Variable("y"), Coefficient(0)),
      Summand(Coefficient(-100), Variable("Yeah"), Coefficient(-13001)),
    ) shouldBe "-2Z + 30 - 100YEAH^-13001 = 0"
  }

  private val default: EquationPrinter[Id] = EquationPrinter.default[Id]
  private val pretty: EquationPrinter[Id]  = EquationPrinter.pretty[Id]

  def printingDefault(head: Summand, tail: Summand*): String =
    default.print(ZeroEquation(NonEmptyList.of(head, tail: _*)))

  def printingPretty(head: Summand, tail: Summand*): String =
    pretty.print(ZeroEquation(NonEmptyList.of(head, tail: _*)))
