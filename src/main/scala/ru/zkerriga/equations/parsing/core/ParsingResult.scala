package ru.zkerriga.equations.parsing.core

sealed abstract class ParsingResult

object ParsingResult:

  case class Success(raw: String) extends ParsingResult

  case class Failure(raw: String, errorTagIndex: Int, description: String) extends ParsingResult
