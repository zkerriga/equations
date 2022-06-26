package ru.zkerriga.equations.processing

import ru.zkerriga.equations.domain.Coefficient
import ru.zkerriga.equations.processing.models.SimplifiedZeroEquation

object Degree {
  def polynomialDegree(equation: SimplifiedZeroEquation): Coefficient =
    equation.summands.keys.maxByOption(_.exponent).map(_.exponent).getOrElse(Coefficient.Zero)
}
