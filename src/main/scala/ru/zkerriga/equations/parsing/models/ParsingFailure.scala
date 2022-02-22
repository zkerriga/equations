package ru.zkerriga.equations.parsing.models

case class ParsingFailure(
  raw: String,
  rightErrorIndex: Int,
  description: String,
)
