package ru.zkerriga.equations

import cats.Monad
import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import ru.zkerriga.equations.parsing.EquationParser

object Main extends IOApp {
  val parser: EquationParser[IO] = EquationParser.make[IO]
  val processor: Processing[IO] = Processing.make[IO](parser)

  def equationsLoop[F[_]: Monad: Console](processor: Processing[F]): F[Unit] =
    for {
      _ <- Console[F].print("Write equation: ")
      rawEquation <- Console[F].readLine
      result <- processor.process(rawEquation)
      _ <- Console[F].println(result)
      _ <- equationsLoop(processor)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    equationsLoop(processor) as ExitCode.Success
}
