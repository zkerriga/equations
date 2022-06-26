package ru.zkerriga.equations

import cats.{Applicative, Monad}
import cats.Show
import cats.data.EitherT
import ru.zkerriga.equations.parsing.EquationParser
import ru.zkerriga.equations.parsing.models.{ErrorMessage, ZeroEquation}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import ru.zkerriga.equations.printer.EquationPrinter

trait Processing[F[_]]:
  def apply(rawEquation: String): F[String]

object Processing:
  private final class Impl[F[_]: Monad](parser: EquationParser[F], printer: EquationPrinter[F])
      extends Processing[F]:
    override def apply(rawEquation: String): F[String] =
      parser.parse(rawEquation) flatMap {
        case Left(error)     => Show[ErrorMessage].show(error).pure[F]
        case Right(equation) => printer.print(equation) // todo
      }

  def make[F[_]: Monad](parser: EquationParser[F], printer: EquationPrinter[F]): Processing[F] =
    Impl[F](parser, printer)
