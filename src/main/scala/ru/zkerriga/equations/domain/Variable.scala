package ru.zkerriga.equations.domain

opaque type Variable = String
object Variable derives CanEqual:
  inline def apply(name: String): Variable = name.toUpperCase

  given Ordering[Variable] = scala.Ordering.String

  final val Default: Variable = "X"
