package ru.zkerriga.equations.parsing

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.implicits._
import cats.syntax.either._
import cats.syntax.monad._
import ru.zkerriga.equations.domain.ZeroEquation
import ru.zkerriga.equations.domain.Coefficient._
import ru.zkerriga.equations.parsing.core.{EquationSides, PreParsers, Parser, ParsingResult}
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
        sidesResult flatMap { case EquationSides(left, right) =>
          val leftBlocks  = PreParsers.separateSummands(left)
          val rightBlocks = PreParsers.separateSummands(right)

          val leftResults  = leftBlocks map Parser.parseSummand
          val rightResults = rightBlocks map Parser.parseSummand

          val (leftParsingResults, leftSummands)   = collectErrors(leftResults)
          val (rigntParsingResults, rightSummands) = collectErrors(rightResults)

          if (
            leftSummands.length === leftResults.length &&
            rightSummands.length === rightResults.length
          )
            ZeroEquation(
              leftSummands ++: rightSummands.map(s => s.copy(multiplier = s.multiplier.toNegative))
            ).asRight
          else
            ErrorMessage
              .generate(
                NonEmptyList.fromListUnsafe(
                  leftParsingResults ::: ParsingResult.Success(" = ") :: rigntParsingResults
                )
              ).asLeft
        }
      }
    }

    private def collectErrors[A](
      results: List[Either[ParsingResult.Failure, (ParsingResult.Success, A)]]
    ): (List[ParsingResult], List[A]) =
      val (pr, a) = results.foldLeft((List.empty[ParsingResult], List.empty[A])) {
        case ((accParsingResults, accA), result) =>
          result match {
            case Left(failure)     => (failure :: accParsingResults, accA)
            case Right(success, a) => (success :: accParsingResults, a :: accA)
          }
      }
      (pr.reverse, a.reverse)

  def make[F[_]: Monad]: EquationParser[F] = Impl[F]
