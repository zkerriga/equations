package ru.zkerriga.equations.parsing

import cats.syntax.either._

package object models {

  opaque type ParsingResult[A] = Either[List[ParsingFailure], A]

  object ParsingResult:
    def success[A](a: A): ParsingResult[A] = a.asRight

}
