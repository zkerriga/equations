package ru.zkerriga.equations.parsing

import cats.Functor
import cats.Show
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.{Monad, MonadError}
import cats.syntax.either.*
import ru.zkerriga.equations.parsing.core.ErrorPrinter
import ru.zkerriga.equations.utils.Raise
import ru.zkerriga.equations.parsing.core.ParsingResult

package object models {

  opaque type ErrorMessage = String
  object ErrorMessage {
    def generate(results: NonEmptyList[ParsingResult]): ErrorMessage =
      ErrorPrinter.showErrors(results)

    def from(failure: ParsingResult.Failure): ErrorMessage =
      generate(NonEmptyList.one(failure))

    given Show[ErrorMessage] = Show.catsShowForString
  }

}
