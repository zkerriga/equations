package ru.zkerriga.equations.processing

import cats.syntax.option.*
import ru.zkerriga.equations.domain.*
import ru.zkerriga.equations.parsing.models.ZeroEquation
import ru.zkerriga.equations.processing.models.{ExponentVariable, SimplifiedZeroEquation}

object Simplification:
  def simplify(equation: ZeroEquation): SimplifiedZeroEquation = SimplifiedZeroEquation {
    equation.summands.foldLeft(Map.empty[ExponentVariable, Coefficient]) { (acc, summand) =>
      val exponentVariable = ExponentVariable(
        if summand.exponent == Coefficient.Zero then Variable.Default else summand.variable,
        summand.exponent,
      )

      acc.updatedWith(exponentVariable) { maybeCoefficient =>
        val newCoefficient = maybeCoefficient.getOrElse(Coefficient.Zero) + summand.multiplier
        if newCoefficient == Coefficient.Zero then None else newCoefficient.some
      }
    }
  }
