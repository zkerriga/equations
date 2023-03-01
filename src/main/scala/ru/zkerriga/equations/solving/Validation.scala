package ru.zkerriga.equations.solving

import ru.zkerriga.equations.processing.models.{ExponentVariable, SimplifiedZeroEquation}
import ru.zkerriga.equations.solving.models.Equation
import cats.data.ValidatedNec
import cats.implicits.*
import cats.data.Validated.*
import ru.zkerriga.equations.domain.*

object Validation:
  type ErrorMessage        = String
  type ValidationResult[A] = ValidatedNec[ErrorMessage, A]

  def validateOnlyOneVariable(
    summands: Map[ExponentVariable, Coefficient]
  ): ValidationResult[Unit] =
    if summands.exists { case (ExponentVariable(name, _), _) => name != Variable.Default } then
      "the solution is only available for one variable".invalidNec
    else ().validNec

  private val PossibleExponents = Set(Coefficient.Zero, Coefficient.One, Coefficient(2))

  def validateExponents(
    summands: Map[ExponentVariable, Coefficient]
  ): ValidationResult[Unit] =
    val allExponentsExpected = summands.forall { case (ExponentVariable(_, exponent), _) =>
      PossibleExponents.contains(exponent)
    }
    if allExponentsExpected then ().validNec
    else
      s"Only the following degrees are available: [${PossibleExponents.mkString(",")}]".invalidNec

  def validate(equation: SimplifiedZeroEquation): Either[ErrorMessage, Equation] =
    (
      validateOnlyOneVariable(equation.summands),
      validateExponents(equation.summands),
    ).mapN { (_, _) =>
      ???
    }
