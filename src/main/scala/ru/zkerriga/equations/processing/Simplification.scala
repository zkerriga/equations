package ru.zkerriga.equations.processing

import cats.syntax.option.*
import ru.zkerriga.equations.domain.*
import ru.zkerriga.equations.parsing.models.ZeroEquation
import ru.zkerriga.equations.processing.models.{ExponentVariable, SimplifiedZeroEquation}
import math.Ordering.Implicits.infixOrderingOps

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

  def simplifyVariableNames(equation: SimplifiedZeroEquation): SimplifiedZeroEquation =
    val anotherVariables: Set[Variable] = equation.summands.foldLeft(Set.empty[Variable]) {
      case (names, (ExponentVariable(name, exponent), _)) =>
        if exponent != Coefficient.Zero then names + name
        else names
    }
    if anotherVariables.size == 1 then
      SimplifiedZeroEquation(
        equation.summands.map { case (ExponentVariable(_, exponent), coefficient) =>
          ExponentVariable(Variable.Default, exponent) -> coefficient
        }
      )
    else equation

  def removeNegativeExponents(equation: SimplifiedZeroEquation): (SimplifiedZeroEquation, Boolean) =
    val minExponent =
      equation.summands.minByOption(_._1.exponent).map(_._1.exponent).getOrElse(Coefficient.Zero)

    val anotherVariableExists = equation.summands.exists(_._1.name != Variable.Default)

    if minExponent < Coefficient.Zero && !anotherVariableExists then
      SimplifiedZeroEquation(equation.summands.map {
        case (ExponentVariable(name, exponent), coefficient) =>
          ExponentVariable(name, exponent - minExponent) -> coefficient
      })          -> true
    else equation -> false
