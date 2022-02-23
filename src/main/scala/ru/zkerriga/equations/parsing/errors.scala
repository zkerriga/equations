package ru.zkerriga.equations.parsing

object errors {
  val EqualSignNotFound   = "The equal sign was not found"
  val NotOnlyOneEqualSign = "There should be only one equal sign"

  val UnexpectedExpression      = "Unexpected expression"
  val UnexpectedEndOfExpression = "Unexpected end of an expression"

  val UnexpectedSign       = "An unexpected sign"
  val UnexpectedMultiplier = "Unexpected coefficient of the summand"
  val UnexpectedVariable   = "Unexpected variable of the summand"
  val UnexpectedExponent   = "Unexpected exponent of the summand"

  val ExponentMustHaveNumber = "The exponent must have a number"

  val VariableExpectedAfterMultiplication = "A variable is expected after multiplication"
}
