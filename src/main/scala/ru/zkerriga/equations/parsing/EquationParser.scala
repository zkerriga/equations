package ru.zkerriga.equations.parsing

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.implicits._
import cats.syntax.either._
import cats.syntax.monad._
import ru.zkerriga.equations.domain.ZeroEquation
import ru.zkerriga.equations.parsing.core.{EquationSides, PreParsers, ParsingResult}
import ru.zkerriga.equations.parsing.models.{ErrorMessage, Summand}
import ru.zkerriga.equations.utils.Raise
import ru.zkerriga.equations.utils.Raise.syntax.*

trait EquationParser[F[_]]:
  def parse(rawEquation: String): F[Either[ErrorMessage, ZeroEquation]]

object EquationParser:
  private final class Impl[F[_]: Monad] extends EquationParser[F]:

    override def parse(rawEquation: String): F[Either[ErrorMessage, ZeroEquation]] = {
      val spaced      = PreParsers.updateSpaces(rawEquation)
      val sidesResult = PreParsers.extractEqualSign(spaced) leftMap ErrorMessage.from

      Applicative[F].pure {
        sidesResult map { case EquationSides(left, right) =>
          val leftBlocks  = PreParsers.separateSummands(left)
          val rightBlocks = PreParsers.separateSummands(right)

          ZeroEquation(List.empty)
        }
      }
    }

  def make[F[_]: Monad]: EquationParser[F] = Impl[F]
