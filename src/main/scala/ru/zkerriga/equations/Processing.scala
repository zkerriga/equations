package ru.zkerriga.equations

import cats.{Monad, Applicative}
import cats.Show
import cats.data.EitherT
import cats.implicits.*
import ru.zkerriga.equations.domain.ZeroEquation
import ru.zkerriga.equations.parsing.EquationParser
import ru.zkerriga.equations.parsing.models.ErrorMessage

trait Processing[F[_]]:
  def process(rawEquation: String): F[String]

object Processing:
  private final class Impl[F[_]: Monad](parser: EquationParser[F]) extends Processing[F]:
    override def process(rawEquation: String): F[String] =
      parser.parse(rawEquation) flatMap {
        case Left(error)     => Applicative[F].pure(Show[ErrorMessage].show(error))
        case Right(equation) => Applicative[F].pure(equation.toString) // todo
      }

  def make[F[_]: Monad](parser: EquationParser[F]): Processing[F] = Impl[F](parser)
