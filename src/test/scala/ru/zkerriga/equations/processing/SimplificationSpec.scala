package ru.zkerriga.equations.processing

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.zkerriga.equations.domain.*
import ru.zkerriga.equations.parsing.models.{Summand, ZeroEquation}
import ru.zkerriga.equations.processing.models.*

class SimplificationSpec extends AnyFlatSpec with Matchers:
  import Simplification._

  "simplify" should "do nothing in simple expression" in {
    simplify(
      ZeroEquation(
        NonEmptyList.of(
          Summand(Coefficient(3), Variable("X"), Coefficient(1)),
          Summand(Coefficient(2), Variable("Y"), Coefficient(0)),
        )
      )
    ).summands shouldBe Map(
      ExponentVariable(Variable("X"), Coefficient(1)) -> Coefficient(3),
      ExponentVariable(Variable("Y"), Coefficient(0)) -> Coefficient(2),
    )
  }

  it should "remove variable if coefficient equal 0" in {
    simplify(
      ZeroEquation(
        NonEmptyList.of(
          Summand(Coefficient(3), Variable("X"), Coefficient(1)),
          Summand(Coefficient(-3), Variable("X"), Coefficient(1)),
          Summand(Coefficient(0), Variable("Y"), Coefficient(15)),
        )
      )
    ).summands shouldBe Map.empty[ExponentVariable, Coefficient]
  }

  it should "calculate coefficients" in {
    simplify(
      ZeroEquation(
        NonEmptyList.of(
          Summand(Coefficient(1), Variable("X"), Coefficient(0)),
          Summand(Coefficient(1), Variable("X"), Coefficient(0)),
          Summand(Coefficient(2), Variable("X"), Coefficient(1)),
          Summand(Coefficient(2), Variable("X"), Coefficient(1)),
          Summand(Coefficient(3), Variable("X"), Coefficient(2)),
          Summand(Coefficient(3), Variable("X"), Coefficient(2)),
        )
      )
    ).summands shouldBe Map(
      ExponentVariable(Variable("X"), Coefficient(0)) -> Coefficient(2),
      ExponentVariable(Variable("X"), Coefficient(1)) -> Coefficient(4),
      ExponentVariable(Variable("X"), Coefficient(2)) -> Coefficient(6),
    )
  }
