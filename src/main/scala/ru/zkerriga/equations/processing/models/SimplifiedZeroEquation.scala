package ru.zkerriga.equations.processing.models

import cats.data.NonEmptyList
import ru.zkerriga.equations.domain.*
import ru.zkerriga.equations.parsing.models.{Summand, ZeroEquation}

case class SimplifiedZeroEquation(summands: Map[ExponentVariable, Coefficient])

object SimplifiedZeroEquation:
  extension (self: SimplifiedZeroEquation)
    def toEquation: ZeroEquation =
      val summands = self.summands.map { case (expVariable, coefficient) =>
        Summand(coefficient, expVariable.name, expVariable.exponent)
      }.toList
      ZeroEquation(
        summands.sortBy(s => (s.exponent, s.variable)) match
          case head :: tail => NonEmptyList(head, tail)
          case Nil =>
            NonEmptyList.one(Summand(Coefficient.Zero, Variable.Default, Coefficient.Zero))
      )
