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
        _ <- printer.print(equation).bufferize(prefix = "Raw equation: ")
        simplified = Simplification.simplify(equation)
        _ <- printer
          .print(simplified.toEquation).bufferize(prefix = "Reduced form: ", nextLine = true)
      } yield ()).runEmptyS

    extension [F[_]: Monad](fs: F[String])
      def bufferize(prefix: String = "", nextLine: Boolean = false): StateT[F, String, Unit] =
        StateT { buffer =>
          fs.map { output =>
            (buffer + (if nextLine then "\n" else "") + prefix + output, ())
          }
        }

  end Impl

  def make[F[_]: Monad](parser: EquationParser[F], printer: EquationPrinter[F]): Processing[F] =
    Impl[F](parser, printer)
