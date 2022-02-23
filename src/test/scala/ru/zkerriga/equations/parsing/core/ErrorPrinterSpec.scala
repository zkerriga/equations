package ru.zkerriga.equations.parsing.core

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.zkerriga.equations.parsing.core.ErrorPrinter.{
  MessageBuilder,
  OffsetMessage,
  makeTagPart,
  showErrors,
}
import ru.zkerriga.equations.parsing.core.ParsingResult

class ErrorPrinterSpec extends AnyFlatSpec with Matchers {

  "MessageBuilder" should "build correct message" in {
    MessageBuilder(
      "1 + 1 + 1 + 1 + 1 + 1 + 1",
      "  ^  ^",
      List(OffsetMessage("Error1", 2), OffsetMessage("Error 2", 5)),
    ).build shouldBe
      """
        |1 + 1 + 1 + 1 + 1 + 1 + 1
        |  ^  ^
        |  Error1
        |     Error 2
        |""".stripMargin
  }

  it should "build correct message if tags doesn't exist" in {
    MessageBuilder("1 + 1", "", List(OffsetMessage("Equal sign not found", 0))).build shouldBe
      """
        |1 + 1
        |
        |Equal sign not found
        |""".stripMargin
  }

  "makeTagsPart" should "correctly mag tags line" in {
    makeTagPart(ParsingResult.Failure("+ 1X", 2, "")) shouldBe "  ^ "
    makeTagPart(ParsingResult.Failure("+ 223X%", 6, "")) shouldBe "      ^"
  }

  it should "print under first char if index doesn't exist" in {
    makeTagPart(ParsingResult.Failure("123", 0, "")) shouldBe "^  "
    makeTagPart(ParsingResult.Failure("1", 0, "")) shouldBe "^"
    makeTagPart(ParsingResult.Failure("", 0, "")) shouldBe ""
  }

  it should "not fail if index invalid" in {
    makeTagPart(ParsingResult.Failure("123", 20, "")) shouldBe "   "
  }

  "showErrors" should "return correct message" in {
    showErrors(NonEmptyList.one(ParsingResult.Failure("1 + + 1", 3, "Error"))) shouldBe
      """
        |1 + + 1
        |   ^
        |   Error
        |""".stripMargin
  }
}
