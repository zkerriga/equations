package ru.zkerriga.equations

import cats.Monad
import cats.implicits._
import ru.zkerriga.equations.parsing.EquationParser

trait Processing[F[_]]:
  def process(rawEquation: String): F[String]

object Processing:
  private final class Impl[F[_]: Monad](parser: EquationParser[F]) extends Processing[F]:
    override def process(rawEquation: String): F[String] =
      for {
        parsingResult <- parser.parse(rawEquation)
      } yield parsingResult.toString()

  def make[F[_]: Monad](parser: EquationParser[F]): Processing[F] = Impl[F](parser)
