package ru.zkerriga.equations.utils

import cats.{FlatMap, Applicative}

trait Raise[F[_], E] extends Raise.ContravariantRaise[F, E]:
  def raise[A](err: E): F[A]

object Raise:
  trait ContravariantRaise[F[_], -E] {
    def raise[A](err: E): F[A]

    def reRaise[A, E1 <: E](fa: F[Either[E1, A]])(using FlatMap[F], Applicative[F]): F[A] =
      FlatMap[F].flatMap(fa)(_.fold(raise[A], Applicative[F].pure))
  }

  def apply[F[_], E](using ev: Raise[F, E]): Raise[F, E] = ev

  object syntax:
    extension [E](err: E) def raise[F[_], A](using raise: Raise[F, E]): F[A] = raise.raise(err)

    extension [E, A](or: Either[E, A])
      def absolve[F[_], EE >: E](using
        raise: Raise[F, EE]
      )(using FlatMap[F], Applicative[F]): F[A] =
        raise.reRaise(Applicative[F].pure(or))

  end syntax
