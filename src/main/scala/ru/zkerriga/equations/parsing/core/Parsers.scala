package ru.zkerriga.equations.parsing.core

import ru.zkerriga.equations.parsing.models.ParsingFailure

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

  def extractEqualSign(raw: String): Either[ParsingFailure, EquationSides[String]] = ???

}
