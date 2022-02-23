package ru.zkerriga.equations.parsing.models

case class ParsingFailure(
  raw: String,
  errorPlaceIndex: Int,
  description: String,
)
