package ru.zkerriga.equations.parsing.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.zkerriga.equations.parsing.errors.{EqualSignNotFound, NotOnlyOneEqualSign}
import ru.zkerriga.equations.parsing.core.ParsingResult

class ParsersSpec extends AnyFlatSpec with Matchers {
  import Parsers._

  "updateSpaces" should "convert many spaces to one" in {
    updateSpaces("X        Y Z 1 = 2 -  \t4\t  +        0") shouldBe "X Y Z 1 = 2 - 4 + 0"
    updateSpaces("    ") shouldBe " "
    updateSpaces("  1  =  1     ") shouldBe " 1 = 1 "
  }

  it should "wrap and unwrap signs with spaces" in {
    updateSpaces("1+2X ^ 1-1X^0=1*X") shouldBe "1 + 2X^1 - 1X^0 = 1 * X"
    updateSpaces("x ^ - 1") shouldBe "x^-1"
  }

  "extractEqualSign" should "get error if = not found" in {
    extractEqualSign("1 + 1") shouldBe Left(ParsingResult.Failure("1 + 1", 0, EqualSignNotFound))
    extractEqualSign("1 - 3 + X^10") shouldBe Left(
      ParsingResult.Failure("1 - 3 + X^10", 0, EqualSignNotFound)
    )
  }

  it should "get error if = sign not only one" in {
    extractEqualSign("1 + 2 = 1 = 3") shouldBe
      Left(ParsingResult.Failure("1 + 2 = 1 = 3", 10, NotOnlyOneEqualSign))

    extractEqualSign("1 + 2 + 3 = = = = 1") shouldBe
      Left(ParsingResult.Failure("1 + 2 + 3 = = = = 1", 12, NotOnlyOneEqualSign))
  }

  it should "get sides" in {
    extractEqualSign("1 = 1") shouldBe Right(EquationSides("1 ", " 1"))
    extractEqualSign("1 + 1 = 4 - 2") shouldBe Right(EquationSides("1 + 1 ", " 4 - 2"))
    extractEqualSign("X^0 = Y^0") shouldBe Right(EquationSides("X^0 ", " Y^0"))
  }

}
