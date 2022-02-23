package ru.zkerriga.equations.parsing.core

import cats.syntax.eq.*
import cats.syntax.either.*
import cats.syntax.option.*
import ru.zkerriga.equations.parsing.errors.*
import ru.zkerriga.equations.parsing.core.ParsingResult

private[parsing] object Parsers {
  def updateSpaces(raw: String): String = {

    extension (raw: String)
      def wrapSign(sign: String): String =
        raw.replaceAll("[ \t]*[" + sign + "][ \t]*", s" $sign ")
      def unwrapSign(sign: String): String =
        raw.replaceAll("[ \t]*[" + sign + "][ \t]*", s"$sign")

    raw
      .replaceAll("\\s+", " ")
      .wrapSign("+")
      .wrapSign("-")
      .wrapSign("=")
      .wrapSign("*")
      .unwrapSign("\\^")
      .replaceAll("\\s*[\\^]\\s*[-]\\s*", "^-")
  }

  def extractEqualSign(raw: String): Either[ParsingResult.Failure, EquationSides[String]] =
    raw.split("=", 3) match {
      case Array(_)           => ParsingResult.Failure(raw, 0, EqualSignNotFound).asLeft
      case Array(left, right) => EquationSides(left, right).asRight
      case Array(first, second, _*) =>
        ParsingResult
          .Failure(raw, first.length + second.length + 1, NotOnlyOneEqualSign).asLeft
    }

}
