package ru.zkerriga.equations

package object domain {

  opaque type Coefficient = BigDecimal
  object Coefficient {
    def apply(n: BigDecimal): Coefficient = n

    extension (c: Coefficient) def toNegative: Coefficient = c * BigDecimal(-1)

    val one: Coefficient  = BigDecimal(1)
    val zero: Coefficient = BigDecimal(0)
  }

  opaque type Variable = String
  object Variable {
    def apply(name: String): Variable = name.toUpperCase

    val default: Variable = "X"
  }

}
