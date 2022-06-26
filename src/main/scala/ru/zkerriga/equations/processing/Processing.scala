package ru.zkerriga.equations.processing

import cats.data.EitherT
import cats.data.StateT
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{Applicative, Monad, Show}
import ru.zkerriga.equations.parsing.EquationParser
import ru.zkerriga.equations.parsing.models.{ErrorMessage, ZeroEquation}
import ru.zkerriga.equations.printer.EquationPrinter

trait Processing[F[_]]:
  def apply(rawEquation: String): F[String]

object Processing:
  private final class Impl[F[_]: Monad](parser: EquationParser[F], printer: EquationPrinter[F])
      extends Processing[F]:
    override def apply(rawEquation: String): F[String] =
      parser.parse(rawEquation) flatMap {
        case Left(error)     => Show[ErrorMessage].show(error).pure[F]
        case Right(equation) => processingOn(equation)
      }

    private def processingOn(equation: ZeroEquation): F[String] =
      (for {
        _ <- printer.print(equation).bufferize("Raw equation: ", newLine = false)
        simplified = Simplification.simplifyVariableNames(Simplification.simplify(equation))
        _ <- printer.print(simplified.toEquation).bufferize("Reduced form: ")
        (withoutNegative, wasCleaned) = Simplification.removeNegativeExponents(simplified)
        _ <-
          if wasCleaned then printer.print(withoutNegative.toEquation).bufferize("Increased: ")
          else StateT.pure(())

        degree = Degree.polynomialDegree(withoutNegative)
        _ <- degree.toString.pure[F].bufferize("Polynomial degree: ")
      } yield ()).runEmptyS

    extension [F[_]: Monad](fs: F[String])
      def bufferize(prefix: String = "", newLine: Boolean = true): StateT[F, String, Unit] =
        StateT { buffer =>
          fs.map { output =>
            (buffer + (if newLine then "\n" else "") + prefix + output, ())
          }
        }

  end Impl

  def make[F[_]: Monad](parser: EquationParser[F], printer: EquationPrinter[F]): Processing[F] =
    Impl[F](parser, printer)
