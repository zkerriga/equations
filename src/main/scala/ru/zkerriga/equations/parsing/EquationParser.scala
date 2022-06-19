package ru.zkerriga.equations.parsing

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.Parallel
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import ru.zkerriga.equations.domain.*
import ru.zkerriga.equations.parsing.core.{EquationSides, Parser, ParsingResult, PreParsers}
import ru.zkerriga.equations.parsing.models.*
import ru.zkerriga.equations.utils.{Delay, Raise}
import ru.zkerriga.equations.utils.Raise.syntax.*
import cats.effect.syntax.concurrent.*
import ru.zkerriga.equations.parsing.errors.SideDoesNotExist

trait EquationParser[F[_]]:
  def parse(rawEquation: String): F[Either[ErrorMessage, ZeroEquation]]

object EquationParser:
  private final class Impl[F[_]: Monad: Delay: Concurrent: Parallel] extends EquationParser[F]:
    override def parse(rawEquation: String): F[Either[ErrorMessage, ZeroEquation]] =
      val spaced = PreParsers.updateSpaces(rawEquation)
      val wrapped = for {
        sides <- EitherT.fromEither[F](
          PreParsers.extractEqualSign(spaced) leftMap ErrorMessage.from
        )
        parPair <- EitherT.right(
          (parseSummandsFrom(sides.left), parseSummandsFrom(sides.right)).parTupled
        ) flatMap ensureEquationIsCorrect

        (leftSummands, rightSummands) = parPair
        summands = leftSummands ::: rightSummands.map(s =>
          s.copy(multiplier = s.multiplier.toNegative)
        )
      } yield ZeroEquation(summands)
      wrapped.value

    private type Parsed[A]  = (List[ParsingResult], List[A])
    private type SidesOf[A] = (A, A)

    private def isCorrect(result: Parsed[Summand]): Boolean = result match
      case (parsingResults, summands) => parsingResults.length == summands.length

    private def addEmptySideError(results: List[ParsingResult]): List[ParsingResult] =
      if results.isEmpty then List(ParsingResult.Failure(" ", 0, SideDoesNotExist))
      else results

    private val ensureEquationIsCorrect: SidesOf[Parsed[Summand]] => EitherT[F, ErrorMessage, SidesOf[List[Summand]]] = {
      case ((leftParsings, leftSummands), (rightParsings, rightSummands)) =>
        val fullLeftParsings  = addEmptySideError(leftParsings)
        val fullRightParsings = addEmptySideError(rightParsings)

        val leftCorrect  = isCorrect(fullLeftParsings -> leftSummands)
        val rightCorrect = isCorrect(fullRightParsings -> rightSummands)

        EitherT.cond(
          test = leftCorrect && rightCorrect,
          right = leftSummands -> rightSummands,
          left = ErrorMessage.generate(
            NonEmptyList.fromListUnsafe(
              fullLeftParsings ::: ParsingResult.Success(" = ") :: fullRightParsings
            )
          ),
        )
    }

    private def parseSummandsFrom(spaced: String): F[Parsed[Summand]] =
      val blocks: List[String] = PreParsers.separateSummands(spaced)
      blocks.parTraverseN(10) { block =>
        summon[Delay[F]].delay(Parser.parseSummand(block))
      } map collectErrors

    private def collectErrors[A](
      results: List[Either[ParsingResult.Failure, (ParsingResult.Success, A)]]
    ): Parsed[A] =
      val (pr, a) = results.foldLeft((List.empty[ParsingResult], List.empty[A])) {
        case ((accParsingResults, accA), result) =>
          result match
            case Left(failure)     => (failure :: accParsingResults, accA)
            case Right(success, a) => (success :: accParsingResults, a :: accA)
      }
      (pr.reverse, a.reverse)

  end Impl

  def make[F[_]: Monad: Delay: Concurrent: Parallel]: EquationParser[F] = Impl[F]
