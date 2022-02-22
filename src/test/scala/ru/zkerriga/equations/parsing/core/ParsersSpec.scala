package ru.zkerriga.equations.parsing.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

}
