package ru.zkerriga.equations.parsing.core

import cats.syntax.eq.*
import cats.data.NonEmptyList
import ru.zkerriga.equations.parsing.core.ParsingResult

private[parsing] object ErrorPrinter {
  case class OffsetMessage(message: String, offset: Int) {
    def show: String = (" " * offset) + message
  }

  case class MessageBuilder(
    equationLine: String,
    tagsLine: String,
    descriptions: List[OffsetMessage],
  )
  object MessageBuilder {
    extension (b: MessageBuilder)
      def addEquationPart(part: String): MessageBuilder =
        b.copy(equationLine = b.equationLine ++ part)

      def addTagPart(part: String): MessageBuilder =
        b.copy(tagsLine = b.tagsLine ++ part)

      def addDescription(offset: Int, description: String): MessageBuilder =
        b.copy(descriptions = OffsetMessage(description, offset) +: b.descriptions)

      def build: String =
        s"""
           |${b.equationLine}
           |${b.tagsLine.stripTrailing()}
           |${b.descriptions.map(_.show).mkString("\n")}
           |""".stripMargin
  }

  def makeTagPart(e: ParsingResult.Failure): String =
    if (e.errorTagIndex < e.raw.length)
      (" " * e.errorTagIndex) + '^' ++ (" " * (e.raw.length - e.errorTagIndex - 1))
    else
      " " * e.raw.length

  def showErrors(errors: NonEmptyList[ParsingResult]): String =
    errors
      .foldLeft(MessageBuilder("", "", List.empty)) { (builder, error) =>
        error match
          case ParsingResult.Success(raw) =>
            builder
              .addEquationPart(raw)
              .addTagPart(" " * raw.length)

          case f @ ParsingResult.Failure(raw, index, description) =>
            builder
              .addEquationPart(raw)
              .addTagPart(makeTagPart(f))
              .addDescription(index + builder.equationLine.length, description)
      }.build
}
