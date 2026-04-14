package hydra.ast

import hydra.core.*

enum Associativity :
   case none extends Associativity
   case left extends Associativity
   case right extends Associativity
   case both extends Associativity

case class BlockStyle(indent: Option[scala.Predef.String], newlineBeforeContent: Boolean, newlineAfterContent: Boolean)

case class BracketExpr(brackets: hydra.ast.Brackets, enclosed: hydra.ast.Expr, style: hydra.ast.BlockStyle)

case class Brackets(open: hydra.ast.Symbol, close: hydra.ast.Symbol)

enum Expr :
   case const(value: hydra.ast.Symbol) extends Expr
   case indent(value: hydra.ast.IndentedExpression) extends Expr
   case op(value: hydra.ast.OpExpr) extends Expr
   case brackets(value: hydra.ast.BracketExpr) extends Expr
   case seq(value: hydra.ast.SeqExpr) extends Expr

case class IndentedExpression(style: hydra.ast.IndentStyle, expr: hydra.ast.Expr)

enum IndentStyle :
   case allLines(value: scala.Predef.String) extends IndentStyle
   case subsequentLines(value: scala.Predef.String) extends IndentStyle

case class Op(symbol: hydra.ast.Symbol, padding: hydra.ast.Padding, precedence: hydra.ast.Precedence,
   associativity: hydra.ast.Associativity)

case class OpExpr(op: hydra.ast.Op, lhs: hydra.ast.Expr, rhs: hydra.ast.Expr)

case class Padding(left: hydra.ast.Ws, right: hydra.ast.Ws)

type Precedence = Int

case class SeqExpr(op: hydra.ast.Op, elements: Seq[hydra.ast.Expr])

type Symbol = scala.Predef.String

enum Ws :
   case none extends Ws
   case space extends Ws
   case break extends Ws
   case breakAndIndent(value: scala.Predef.String) extends Ws
   case doubleBreak extends Ws
