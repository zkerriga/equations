package ru.zkerriga.equations.parsing.core

import cats.syntax.option._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.zkerriga.equations.parsing.models.Summand
import ru.zkerriga.equations.domain._

class SummandBuilderSpec extends AnyFlatSpec with Matchers {
  import SummandBuilder._

  "SummandBuilder" should "builds from full Summand" in {
    SummandBuilder(
      multiplierSign = Sign.Minus.some,
      multiplier = Coefficient(BigDecimal(4)).some,
      variable = Variable("Y").some,
      exponent = RawExponent(Sign.Plus.some, Coefficient(BigDecimal(2))).some,
    ).build shouldBe Summand(
      multiplier = Coefficient(BigDecimal(-4)),
      variable = Variable("Y"),
      exponent = Coefficient(BigDecimal(2)),
    )
  }

  it should "builds from empty coefficients" in {
    SummandBuilder(
      multiplierSign = None,
      multiplier = None,
      variable = Variable("R").some,
      exponent = None,
    ).build shouldBe Summand(
      multiplier = Coefficient(BigDecimal(1)),
      variable = Variable("R"),
      exponent = Coefficient(BigDecimal(1)),
    )
  }

  it should "builds from empty variable" in {
    SummandBuilder(
      multiplierSign = None,
      multiplier = Coefficient(BigDecimal(4)).some,
      variable = None,
      exponent = None,
    ).build shouldBe Summand(
      multiplier = Coefficient(BigDecimal(4)),
      variable = Variable("X"),
      exponent = Coefficient(BigDecimal(0)),
    )
  }
}
