package hydra.parsing

import hydra.core.*

case class ParseError(message: scala.Predef.String, remainder: scala.Predef.String)

enum ParseResult[A] :
   case success(value: hydra.parsing.ParseSuccess[A]) extends ParseResult[A]
   case failure(value: hydra.parsing.ParseError) extends ParseResult[A]

case class ParseSuccess[A](value: A, remainder: scala.Predef.String)

type Parser [A] = (scala.Predef.String => hydra.parsing.ParseResult[A])
