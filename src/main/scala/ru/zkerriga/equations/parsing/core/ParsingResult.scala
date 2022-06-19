package ru.zkerriga.equations.parsing.core

enum ParsingResult:
  case Success(raw: String)
  case Failure(raw: String, errorTagIndex: Int, description: String)
