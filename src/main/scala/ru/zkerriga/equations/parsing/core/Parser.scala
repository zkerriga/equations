package ru.zkerriga.equations.parsing.core

import cats.syntax.either.*
import cats.syntax.eq.*
import cats.data.NonEmptyList
import ru.zkerriga.equations.domain.{Coefficient, Variable}
import ru.zkerriga.equations.parsing.models.Summand
import ru.zkerriga.equations.parsing.errors.*
import ru.zkerriga.equations.parsing.core.SummandBuilder.*

object Parser {

  def parseNumber(
    raw: List[Char],
    acc: NonEmptyList[Char],
    dotFlag: Boolean = false,
  ): (Coefficient, List[Char]) = raw match {
    case Nil => Coefficient(BigDecimal.exact(acc.reverse.toList.mkString)) -> Nil
    case ch :: tail =>
      if (ch.isDigit || (ch === '.' && dotFlag === false))
        parseNumber(tail, ch :: acc, dotFlag || ch === '.')
      else Coefficient(BigDecimal.exact(acc.reverse.toList.mkString)) -> raw
  }

  def parseExponent(raw: List[Char]): Either[String, (Coefficient, List[Char])] = raw match {
    case Nil => ExponentMustHaveNumber.asLeft
    case ch :: tail =>
      if (ch === '-') parseNumber(tail, NonEmptyList.of('0', '-')).asRight
      else if (ch === '+') parseNumber(tail, NonEmptyList.one('0')).asRight
      else if (ch.isDigit) parseNumber(tail, NonEmptyList.one(ch)).asRight
      else ExponentMustHaveNumber.asLeft
  }

  def parseVariable(raw: List[Char], acc: NonEmptyList[Char]): (Variable, List[Char]) = raw match {
    case Nil => Variable(acc.reverse.toList.mkString) -> Nil
    case ch :: tail =>
      if (ch.isLetter || ch === '_') parseVariable(tail, ch :: acc)
      else Variable(acc.reverse.toList.mkString) -> raw
  }

  def parseStarVariable(raw: List[Char]): Either[String, (Variable, List[Char])] =
    raw.dropWhile(_ === ' ') match {
      case Nil => VariableExpectedAfterMultiplication.asLeft
      case ch :: tail =>
        if (ch.isLetter) parseVariable(tail, NonEmptyList.one(ch)).asRight
        else VariableExpectedAfterMultiplication.asLeft
    }

  def parseSummand(
    rawSummand: String
  ): Either[ParsingResult.Failure, (ParsingResult.Success, Summand)] = {

    def fail(index: Int, description: String): Either[ParsingResult.Failure, Nothing] =
      ParsingResult.Failure(rawSummand, index, description).asLeft
    def success(builder: SummandBuilder): Either[Nothing, (ParsingResult.Success, Summand)] =
      (ParsingResult.Success(rawSummand), builder.build).asRight

    @scala.annotation.tailrec
    def parse(
      rawLeft: List[Char],
      builder: SummandBuilder,
      index: Int,
    ): Either[ParsingResult.Failure, (ParsingResult.Success, Summand)] = rawLeft match {
      case Nil =>
        if (builder.state < State.Multiplier) fail(rawSummand.length - 1, UnexpectedEndOfExpression)
        else success(builder)

      case ch :: tail =>
        ch match {
          case '+' | '-' =>
            if (builder.state === State.Empty)
              parse(
                tail,
                builder.addMultiplierSign(if (ch === '+') Sign.Plus else Sign.Minus),
                index + 1,
              )
            else fail(index, UnexpectedSign)

          case c if c.isDigit =>
            if (builder.state < State.Multiplier) {
              val (coef, nextTail) = parseNumber(tail, NonEmptyList.one(ch))
              parse(nextTail, builder.addMultiplier(coef), index + rawLeft.length - nextTail.length)
            } else fail(index, UnexpectedMultiplier)

          case ' ' => parse(tail, builder, index + 1)

          case c if c.isLetter =>
            if (builder.state < State.Variable) {
              val (variable, nextTail) = parseVariable(tail, NonEmptyList.one(ch))
              parse(
                nextTail,
                builder.addVariable(variable),
                index + rawLeft.length - nextTail.length,
              )
            } else fail(index, UnexpectedVariable)

          case '^' =>
            if (builder.state === State.Variable)
              parseExponent(tail) match {
                case Left(error) => fail(index, error)
                case Right((exp, nextTail)) =>
                  parse(
                    nextTail,
                    builder.addExponent(exp),
                    index + rawLeft.length - nextTail.length,
                  )
              }
            else fail(index, UnexpectedExponent)

          case '*' =>
            if (builder.state === State.Multiplier) {
              parseStarVariable(tail) match {
                case Left(error) => fail(index, error)
                case Right((variable, nextTail)) =>
                  parse(
                    nextTail,
                    builder.addVariable(variable),
                    index + rawLeft.length - nextTail.length,
                  )
              }
            } else fail(index, UnexpectedVariable)

          case _ => fail(index, UnexpectedExpression)
        }
    }

    parse(rawSummand.toList, SummandBuilder.empty, index = 0)
  }
}
