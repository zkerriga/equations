package ru.zkerriga.equations.parsing

import cats.Monad
import cats.implicits._
import ru.zkerriga.equations.domain.ZeroEquation
import ru.zkerriga.equations.parsing.core.Parsers
import ru.zkerriga.equations.parsing.models.{ParsingFailure, Summand}

trait EquationParser[F[_]] {
  def parse(equation: String): F[ZeroEquation]
}

object EquationParser {
  private final class Impl[F[_]](using Monad[F]) extends EquationParser[F]:
    override def parse(equation: String): F[ZeroEquation] =
      for {
        spaced <- Parsers.updateSpaces(equation).pure[F]
        _      <- println(spaced).pure
      } yield ZeroEquation(List.empty)

  def make[F[_]: Monad]: EquationParser[F] = new Impl[F]
}
