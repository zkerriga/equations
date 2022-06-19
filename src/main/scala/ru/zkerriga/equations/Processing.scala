package ru.zkerriga.equations

import cats.{Monad, Applicative}
import cats.Show
import cats.data.EitherT
import ru.zkerriga.equations.parsing.EquationParser
import ru.zkerriga.equations.parsing.models.{ErrorMessage, ZeroEquation}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*

trait Processing[F[_]]:
  def process(rawEquation: String): F[String]

object Processing:
  private final class Impl[F[_]: Monad](parser: EquationParser[F]) extends Processing[F]:
    override def process(rawEquation: String): F[String] =
      parser.parse(rawEquation) flatMap {
        case Left(error)     => Show[ErrorMessage].show(error).pure[F]
        case Right(equation) => equation.toString.pure[F] // todo
      }

  def make[F[_]: Monad](parser: EquationParser[F]): Processing[F] = Impl[F](parser)
