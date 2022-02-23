package ru.zkerriga.equations.parsing.core

import ru.zkerriga.equations.parsing.models.Summand

object Parser {

  def parseSummand(
    rawSummand: String
  ): Either[ParsingResult.Failure, (ParsingResult.Success, Summand)] = ???
}
