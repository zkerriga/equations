package ru.zkerriga.equations.parsing

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.implicits._
import ru.zkerriga.equations.domain.ZeroEquation
import ru.zkerriga.equations.parsing.core.{EquationSides, Parsers, ParsingResult}
import ru.zkerriga.equations.parsing.models.{ErrorMessage, Summand}
import ru.zkerriga.equations.utils.Raise
import ru.zkerriga.equations.utils.Raise.syntax.*

trait EquationParser[F[_]]:
  def parse(rawEquation: String): F[Either[ErrorMessage, ZeroEquation]]

object EquationParser:
  private final class Impl[F[_]: Monad] extends EquationParser[F]:

    override def parse(rawEquation: String): F[Either[ErrorMessage, ZeroEquation]] = {
      val spaced = Parsers.updateSpaces(rawEquation)

      Applicative[F].pure(Right(ZeroEquation(List.empty)))
    }

  /*(for {
      sides <- prepareInput[ParsingF](rawEquation)

      zeroEquation <- Applicative[ParsingF].pure(ZeroEquation(List.empty))
    } yield zeroEquation).result*/

  /*
    private def prepareInput[G[_]](
      rawEquation: String
    )(using Raise[G, ParsingResult], Monad[G]): G[EquationSides[String]] =
      for {
        spaced <- Parsers.updateSpaces(rawEquation).pure[G]
        sides  <- Parsers.extractEqualSign(spaced).absolve
      } yield sides
   */

  def make[F[_]: Monad]: EquationParser[F] = Impl[F]
