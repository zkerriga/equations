package ru.zkerriga.equations.parsing

object errors {
  inline val EqualSignNotFound   = "The equal sign was not found"
  inline val NotOnlyOneEqualSign = "There should be only one equal sign"

  inline val UnexpectedExpression      = "Unexpected expression"
  inline val UnexpectedEndOfExpression = "Unexpected end of an expression"

  inline val UnexpectedSign       = "An unexpected sign"
  inline val UnexpectedMultiplier = "Unexpected coefficient of the summand"
  inline val UnexpectedVariable   = "Unexpected variable of the summand"
  inline val UnexpectedExponent   = "Unexpected exponent of the summand"

  inline val ExponentMustHaveNumber = "The exponent must have a number"

  inline val VariableExpectedAfterMultiplication = "A variable is expected after multiplication"
}
