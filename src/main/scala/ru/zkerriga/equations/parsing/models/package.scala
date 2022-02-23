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

    given Show[ErrorMessage] = Show.catsShowForString
  }
  /*

  opaque type ParsingT[F[_], A] = EitherT[F, NonEmptyList[ParsingResult], A]
  object ParsingT {
    given [F[_]](using monad: Monad[F]): Monad[[A] =>> ParsingT[F, A]] =
      EitherT.catsDataMonadErrorForEitherT[F, NonEmptyList[ParsingResult]]

    given [F[_]](using monad: Monad[F]): Raise[[A] =>> ParsingT[F, A], ParsingResult] =
      new Raise[[A] =>> ParsingT[F, A], ParsingResult] {
        override def raise[A](err: ParsingFailure): ParsingT[F, A] =
          EitherT.leftT[F, A](NonEmptyList.one(err))
      }

    extension [F[_], A](valueT: ParsingT[F, A])(using Functor[F])
      def result: F[Either[ErrorMessage, A]] =
        valueT.leftMap(ErrorMessage.generate).value
  }
   */

}
