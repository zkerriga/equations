package ru.zkerriga.equations.domain

opaque type Coefficient = BigDecimal
object Coefficient derives CanEqual:
  inline def apply(n: BigDecimal): Coefficient = n

  extension (c: Coefficient)
    def toNegative: Coefficient = c * MinusOne
    def isNegative: Boolean     = c < 0
    def absoluteString: String  = c.abs.toString()

    def +(other: Coefficient): Coefficient = c + other
    def -(other: Coefficient): Coefficient = c - other

  given Ordering[Coefficient] = scala.Ordering.BigDecimal

  final val One: Coefficient      = BigDecimal(1)
  final val MinusOne: Coefficient = BigDecimal(-1)
  final val Zero: Coefficient     = BigDecimal(0)
