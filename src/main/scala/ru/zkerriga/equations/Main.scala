package ru.zkerriga.equations

import cats.Monad
import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import ru.zkerriga.equations.parsing.EquationParser

object Main extends IOApp:
  def equationsLoop[F[_]: Monad](processing: Processing[F])(using console: Console[F]): F[Unit] =
    for
      _           <- console.print("Write equation: ")
      rawEquation <- console.readLine
      result      <- processing(rawEquation)
      _           <- console.println(result)
      _           <- equationsLoop(processing)
    yield ()

  override def run(args: List[String]): IO[ExitCode] =
    val parser: EquationParser[IO] = EquationParser.make[IO]
    val processor: Processing[IO]  = Processing.make[IO](parser)
    equationsLoop(processor) as ExitCode.Success
