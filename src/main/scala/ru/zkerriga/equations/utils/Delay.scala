package ru.zkerriga.equations.utils

import cats.effect.IO

trait Delay[F[_]]:
  def delay[A](a: => A): F[A]

object Delay:
  given Delay[IO] with {
    def delay[A](a: => A): IO[A] = IO.delay(a)
  }
