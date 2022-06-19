package ru.zkerriga.equations.parsing.models

import cats.Show
import cats.data.NonEmptyList
import ru.zkerriga.equations.parsing.core.{ErrorPrinter, ParsingResult}

opaque type ErrorMessage = String
object ErrorMessage:
  def generate(results: NonEmptyList[ParsingResult]): ErrorMessage =
    ErrorPrinter.showErrors(results)

  def from(failure: ParsingResult.Failure): ErrorMessage =
    generate(NonEmptyList.one(failure))

  given Show[ErrorMessage] = Show.catsShowForString
