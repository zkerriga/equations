package ru.zkerriga.equations.parsing.models

import cats.data.NonEmptyList

case class ZeroEquation(summands: NonEmptyList[Summand])
