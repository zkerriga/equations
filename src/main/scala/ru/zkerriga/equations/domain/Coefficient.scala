package ru.zkerriga.equations.domain

opaque type Coefficient = BigDecimal
object Coefficient:
  inline def apply(n: BigDecimal): Coefficient = n

  extension (c: Coefficient) def toNegative: Coefficient = c * BigDecimal(-1)

  final val One: Coefficient  = BigDecimal(1)
  final val Zero: Coefficient = BigDecimal(0)
