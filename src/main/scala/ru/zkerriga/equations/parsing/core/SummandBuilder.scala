package ru.zkerriga.equations.parsing.core

import ru.zkerriga.equations.domain.{Coefficient, Variable}
import ru.zkerriga.equations.domain.Coefficient.*
import ru.zkerriga.equations.domain.Variable.*
import ru.zkerriga.equations.parsing.models.Summand
import ru.zkerriga.equations.parsing.core.SummandBuilder.{RawExponent, Sign}

case class SummandBuilder(
  multiplierSign: Option[Sign],
  multiplier: Option[Coefficient],
  variable: Option[Variable],
  exponent: Option[RawExponent],
)

object SummandBuilder {

  enum Sign:
    case Minus, Plus

  case class RawExponent(sign: Option[Sign], k: Coefficient)

  extension (b: SummandBuilder)
    def build: Summand =
      Summand(
        multiplier = {
          val coef = b.multiplier.getOrElse(Coefficient.one)
          b.multiplierSign match {
            case Some(Sign.Minus) => coef.toNegative
            case _                => coef
          }
        },
        variable = b.variable.getOrElse(Variable.default),
        exponent = {
          if (b.variable.isDefined) b.exponent.fold(Coefficient.one) { rawExp =>
            rawExp.sign match {
              case Some(Sign.Minus) => rawExp.k.toNegative
              case _                => rawExp.k
            }
          }
          else Coefficient.zero
        },
      )

}
