package ru.zkerriga.equations.parsing.core

import cats.syntax.option.*
import ru.zkerriga.equations.domain.*
import ru.zkerriga.equations.parsing.models.Summand
import ru.zkerriga.equations.parsing.core.SummandBuilder.{Sign, State}

import scala.annotation.targetName

case class SummandBuilder(
  state: State = State.Empty,
  multiplierSign: Option[Sign],
  multiplier: Option[Coefficient],
  variable: Option[Variable],
  exponent: Option[Coefficient],
)

object SummandBuilder:
  final val Empty: SummandBuilder = SummandBuilder(State.Empty, None, None, None, None)

  enum State(level: Int):
    case Empty          extends State(0)
    case MultiplierSign extends State(2)
    case Multiplier     extends State(4)
    case Variable       extends State(6)
    case Exponent       extends State(8)

  object State {
    extension (s: State) def <(other: State): Boolean = s.ordinal < other.ordinal
  }

  enum Sign:
    case Minus, Plus

  extension (b: SummandBuilder)
    def addMultiplierSign(sign: Sign): SummandBuilder =
      b.copy(state = State.MultiplierSign, multiplierSign = sign.some)

    def addMultiplier(c: Coefficient): SummandBuilder =
      b.copy(state = State.Multiplier, multiplier = c.some)

    def addVariable(v: Variable): SummandBuilder =
      b.copy(state = State.Variable, variable = v.some)

    def addExponent(exp: Coefficient): SummandBuilder =
      b.copy(state = State.Exponent, exponent = exp.some)

    def build: Summand =
      Summand(
        multiplier = {
          val coefficient = b.multiplier.getOrElse(Coefficient.One)
          b.multiplierSign match
            case Some(Sign.Minus) => coefficient.toNegative
            case _                => coefficient
        },
        variable = b.variable getOrElse Variable.Default,
        exponent =
          if b.variable.isDefined then b.exponent getOrElse Coefficient.One
          else Coefficient.Zero,
      )
