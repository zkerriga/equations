package ru.zkerriga.equations.domain

opaque type Variable = String
object Variable:
  inline def apply(name: String): Variable = name.toUpperCase

  final val Default: Variable = "X"
