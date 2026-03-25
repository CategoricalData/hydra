package hydra.encode.ast

import hydra.ast.*

import hydra.core.*

import hydra.lib.lists

import hydra.lib.maybes

def associativity(v1: hydra.ast.Associativity): hydra.core.Term =
  v1 match
  case hydra.ast.Associativity.none => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Associativity", hydra.core.Field("none", hydra.core.Term.unit)))
  case hydra.ast.Associativity.left => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Associativity", hydra.core.Field("left", hydra.core.Term.unit)))
  case hydra.ast.Associativity.right => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Associativity", hydra.core.Field("right", hydra.core.Term.unit)))
  case hydra.ast.Associativity.both => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Associativity", hydra.core.Field("both", hydra.core.Term.unit)))

def blockStyle(x: hydra.ast.BlockStyle): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.ast.BlockStyle", Seq(hydra.core.Field("indent", hydra.core.Term.maybe(hydra.lib.maybes.map[scala.Predef.String,
     hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.indent))),
     hydra.core.Field("newlineBeforeContent", hydra.core.Term.literal(hydra.core.Literal.boolean(x.newlineBeforeContent))),
     hydra.core.Field("newlineAfterContent", hydra.core.Term.literal(hydra.core.Literal.boolean(x.newlineAfterContent))))))

def bracketExpr(x: hydra.ast.BracketExpr): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.ast.BracketExpr", Seq(hydra.core.Field("brackets", hydra.encode.ast.brackets(x.brackets)),
     hydra.core.Field("enclosed", hydra.encode.ast.expr(x.enclosed)), hydra.core.Field("style", hydra.encode.ast.blockStyle(x.style)))))

def brackets(x: hydra.ast.Brackets): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.ast.Brackets", Seq(hydra.core.Field("open", hydra.encode.ast.symbol(x.open)),
     hydra.core.Field("close", hydra.encode.ast.symbol(x.close)))))

def expr(v1: hydra.ast.Expr): hydra.core.Term =
  v1 match
  case hydra.ast.Expr.const(v_Expr_const_y) => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Expr",
     hydra.core.Field("const", hydra.encode.ast.symbol(v_Expr_const_y))))
  case hydra.ast.Expr.indent(v_Expr_indent_y) => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Expr",
     hydra.core.Field("indent", hydra.encode.ast.indentedExpression(v_Expr_indent_y))))
  case hydra.ast.Expr.op(v_Expr_op_y) => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Expr",
     hydra.core.Field("op", hydra.encode.ast.opExpr(v_Expr_op_y))))
  case hydra.ast.Expr.brackets(v_Expr_brackets_y) => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Expr",
     hydra.core.Field("brackets", hydra.encode.ast.bracketExpr(v_Expr_brackets_y))))
  case hydra.ast.Expr.seq(v_Expr_seq_y) => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Expr",
     hydra.core.Field("seq", hydra.encode.ast.seqExpr(v_Expr_seq_y))))

def indentStyle(v1: hydra.ast.IndentStyle): hydra.core.Term =
  v1 match
  case hydra.ast.IndentStyle.allLines(v_IndentStyle_allLines_y) => hydra.core.Term.union(hydra.core.Injection("hydra.ast.IndentStyle",
     hydra.core.Field("allLines", hydra.core.Term.literal(hydra.core.Literal.string(v_IndentStyle_allLines_y)))))
  case hydra.ast.IndentStyle.subsequentLines(v_IndentStyle_subsequentLines_y) => hydra.core.Term.union(hydra.core.Injection("hydra.ast.IndentStyle",
     hydra.core.Field("subsequentLines", hydra.core.Term.literal(hydra.core.Literal.string(v_IndentStyle_subsequentLines_y)))))

def indentedExpression(x: hydra.ast.IndentedExpression): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.ast.IndentedExpression", Seq(hydra.core.Field("style",
     hydra.encode.ast.indentStyle(x.style)), hydra.core.Field("expr", hydra.encode.ast.expr(x.expr)))))

def op(x: hydra.ast.Op): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.ast.Op", Seq(hydra.core.Field("symbol", hydra.encode.ast.symbol(x.symbol)),
     hydra.core.Field("padding", hydra.encode.ast.padding(x.padding)), hydra.core.Field("precedence",
     hydra.encode.ast.precedence(x.precedence)), hydra.core.Field("associativity", hydra.encode.ast.associativity(x.associativity)))))

def opExpr(x: hydra.ast.OpExpr): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.ast.OpExpr", Seq(hydra.core.Field("op", hydra.encode.ast.op(x.op)),
     hydra.core.Field("lhs", hydra.encode.ast.expr(x.lhs)), hydra.core.Field("rhs", hydra.encode.ast.expr(x.rhs)))))

def padding(x: hydra.ast.Padding): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.ast.Padding", Seq(hydra.core.Field("left", hydra.encode.ast.ws(x.left)),
     hydra.core.Field("right", hydra.encode.ast.ws(x.right)))))

def precedence(x: hydra.ast.Precedence): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.ast.Precedence", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x)))))

def seqExpr(x: hydra.ast.SeqExpr): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.ast.SeqExpr", Seq(hydra.core.Field("op", hydra.encode.ast.op(x.op)),
     hydra.core.Field("elements", hydra.core.Term.list(hydra.lib.lists.map[hydra.ast.Expr, hydra.core.Term](hydra.encode.ast.expr)(x.elements))))))

def symbol(x: hydra.ast.Symbol): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.ast.Symbol", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def ws(v1: hydra.ast.Ws): hydra.core.Term =
  v1 match
  case hydra.ast.Ws.none => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Ws", hydra.core.Field("none", hydra.core.Term.unit)))
  case hydra.ast.Ws.space => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Ws", hydra.core.Field("space", hydra.core.Term.unit)))
  case hydra.ast.Ws.break => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Ws", hydra.core.Field("break", hydra.core.Term.unit)))
  case hydra.ast.Ws.breakAndIndent(v_Ws_breakAndIndent_y) => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Ws",
     hydra.core.Field("breakAndIndent", hydra.core.Term.literal(hydra.core.Literal.string(v_Ws_breakAndIndent_y)))))
  case hydra.ast.Ws.doubleBreak => hydra.core.Term.union(hydra.core.Injection("hydra.ast.Ws", hydra.core.Field("doubleBreak", hydra.core.Term.unit)))
