package ru.zkerriga.equations.parsing.models

import ru.zkerriga.equations.domain.{Coefficient, Variable}

final case class Summand(
  multiplier: Coefficient,
  variable: Variable,
  exponent: Coefficient,
)
