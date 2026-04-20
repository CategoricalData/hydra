package hydra.serialization

import hydra.ast.*

import hydra.util.*

lazy val angleBraces: hydra.ast.Brackets = hydra.ast.Brackets("<", ">")

def angleBracesList(style: hydra.ast.BlockStyle)(els: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ast.Expr](els))(hydra.serialization.cst("<>"))(hydra.serialization.brackets(hydra.serialization.angleBraces)(style)(hydra.serialization.commaSep(style)(els)))

def bracesListAdaptive(els: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  {
  lazy val inlineList: hydra.ast.Expr = hydra.serialization.curlyBracesList(None)(hydra.serialization.inlineStyle)(els)
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.gt[Int](hydra.serialization.expressionLength(inlineList))(70))(hydra.serialization.curlyBracesList(None)(hydra.serialization.halfBlockStyle)(els))(inlineList)
}

def bracketList(style: hydra.ast.BlockStyle)(els: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ast.Expr](els))(hydra.serialization.cst("[]"))(hydra.serialization.brackets(hydra.serialization.squareBrackets)(style)(hydra.serialization.commaSep(style)(els)))

def bracketListAdaptive(els: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  {
  lazy val inlineList: hydra.ast.Expr = hydra.serialization.bracketList(hydra.serialization.inlineStyle)(els)
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.gt[Int](hydra.serialization.expressionLength(inlineList))(70))(hydra.serialization.bracketList(hydra.serialization.halfBlockStyle)(els))(inlineList)
}

def brackets(br: hydra.ast.Brackets)(style: hydra.ast.BlockStyle)(e: hydra.ast.Expr): hydra.ast.Expr = hydra.ast.Expr.brackets(hydra.ast.BracketExpr(br,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   e, style))

def commaSep(v1: hydra.ast.BlockStyle)(v2: Seq[hydra.ast.Expr]): hydra.ast.Expr = hydra.serialization.symbolSep(",")(v1)(v2)

def cst(s: scala.Predef.String): hydra.ast.Expr = hydra.ast.Expr.const(hydra.serialization.sym(s))

def curlyBlock(style: hydra.ast.BlockStyle)(e: hydra.ast.Expr): hydra.ast.Expr = hydra.serialization.curlyBracesList(None)(style)(Seq(e))

lazy val curlyBraces: hydra.ast.Brackets = hydra.ast.Brackets("{", "}")

def curlyBracesList(msymb: Option[scala.Predef.String])(style: hydra.ast.BlockStyle)(els: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ast.Expr](els))(hydra.serialization.cst("{}"))(hydra.serialization.brackets(hydra.serialization.curlyBraces)(style)(hydra.serialization.symbolSep(hydra.lib.maybes.fromMaybe[scala.Predef.String](",")(msymb))(style)(els)))

def customIndent(idt: scala.Predef.String)(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.cat(hydra.lib.lists.intersperse[scala.Predef.String]("\n")(hydra.lib.lists.map[scala.Predef.String,
     scala.Predef.String]((line: scala.Predef.String) => hydra.lib.strings.cat2(idt)(line))(hydra.lib.strings.lines(s))))

def customIndentBlock(idt: scala.Predef.String)(els: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  {
  lazy val idtOp: hydra.ast.Op = hydra.ast.Op(hydra.serialization.sym(""), hydra.ast.Padding(hydra.ast.Ws.space,
     hydra.ast.Ws.breakAndIndent(idt)), 0, hydra.ast.Associativity.none)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ast.Expr](hydra.serialization.cst(""))((head: hydra.ast.Expr) =>
    hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ast.Expr](els))(1))(head)(hydra.serialization.ifx(idtOp)(head)(hydra.serialization.newlineSep(hydra.lib.lists.drop[hydra.ast.Expr](1)(els)))))(hydra.lib.lists.maybeHead[hydra.ast.Expr](els))
}

def dotSep(v1: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.serialization.sep(hydra.ast.Op(hydra.serialization.sym("."), hydra.ast.Padding(hydra.ast.Ws.none,
     hydra.ast.Ws.none), 0, hydra.ast.Associativity.none))(v1)

def doubleNewlineSep(v1: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.serialization.sep(hydra.ast.Op(hydra.serialization.sym(""), hydra.ast.Padding(hydra.ast.Ws.break,
     hydra.ast.Ws.break), 0, hydra.ast.Associativity.none))(v1)

lazy val doubleSpace: scala.Predef.String = "  "

def expressionLength(e: hydra.ast.Expr): Int =
  {
  def symbolLength(s: hydra.ast.Symbol): Int = hydra.lib.strings.length(s)
  def wsLength(ws: hydra.ast.Ws): Int =
    ws match
    case hydra.ast.Ws.none => 0
    case hydra.ast.Ws.space => 1
    case hydra.ast.Ws.break => 10000
    case hydra.ast.Ws.breakAndIndent(v_Ws_breakAndIndent_s) => 10000
    case hydra.ast.Ws.doubleBreak => 10000
  def blockStyleLength(style: hydra.ast.BlockStyle): Int =
    {
    lazy val mindentLen: Int = hydra.lib.maybes.maybe[Int, scala.Predef.String](0)(hydra.lib.strings.length)(style.indent)
    lazy val nlBeforeLen: Int = hydra.lib.logic.ifElse[Int](style.newlineBeforeContent)(1)(0)
    lazy val nlAfterLen: Int = hydra.lib.logic.ifElse[Int](style.newlineAfterContent)(1)(0)
    hydra.lib.math.add(mindentLen)(hydra.lib.math.add(nlBeforeLen)(nlAfterLen))
  }
  def bracketsLength(brackets: hydra.ast.Brackets): Int = hydra.lib.math.add(symbolLength(brackets.open))(symbolLength(brackets.close))
  def bracketExprLength(be: hydra.ast.BracketExpr): Int =
    hydra.lib.math.add(bracketsLength(be.brackets))(hydra.lib.math.add(hydra.serialization.expressionLength(be.enclosed))(blockStyleLength(be.style)))
  def indentedExpressionLength(ie: hydra.ast.IndentedExpression): Int =
    {
    lazy val baseLen: Int = hydra.serialization.expressionLength(ie.expr)
    lazy val indentLen: Int = ie.style match
      case hydra.ast.IndentStyle.allLines(v_IndentStyle_allLines_s) => hydra.lib.strings.length(v_IndentStyle_allLines_s)
      case hydra.ast.IndentStyle.subsequentLines(v_IndentStyle_subsequentLines_s) => hydra.lib.strings.length(v_IndentStyle_subsequentLines_s)
    hydra.lib.math.add(baseLen)(indentLen)
  }
  def opLength(op: hydra.ast.Op): Int =
    {
    lazy val symLen: Int = symbolLength(op.symbol)
    lazy val padding: hydra.ast.Padding = (op.padding)
    lazy val leftLen: Int = wsLength(padding.left)
    lazy val rightLen: Int = wsLength(padding.right)
    hydra.lib.math.add(symLen)(hydra.lib.math.add(leftLen)(rightLen))
  }
  def opExprLength(oe: hydra.ast.OpExpr): Int =
    {
    lazy val opLen: Int = opLength(oe.op)
    lazy val leftLen: Int = hydra.serialization.expressionLength(oe.lhs)
    lazy val rightLen: Int = hydra.serialization.expressionLength(oe.rhs)
    hydra.lib.math.add(opLen)(hydra.lib.math.add(leftLen)(rightLen))
  }
  def seqExprLength(se: hydra.ast.SeqExpr): Int =
    {
    lazy val sopLen: Int = opLength(se.op)
    lazy val elementLens: Seq[Int] = hydra.lib.lists.map[hydra.ast.Expr, Int](hydra.serialization.expressionLength)(se.elements)
    lazy val totalElLen: Int = hydra.lib.lists.foldl[Int, Int](hydra.lib.math.add)(0)(elementLens)
    lazy val numSeps: Int = hydra.lib.math.sub(hydra.lib.lists.length[hydra.ast.Expr](se.elements))(1)
    hydra.lib.math.add(totalElLen)(hydra.lib.math.mul(sopLen)(hydra.lib.logic.ifElse[Int](hydra.lib.equality.gt[Int](numSeps)(0))(numSeps)(0)))
  }
  e match
    case hydra.ast.Expr.const(v_Expr_const_s) => symbolLength(v_Expr_const_s)
    case hydra.ast.Expr.indent(v_Expr_indent_ie) => indentedExpressionLength(v_Expr_indent_ie)
    case hydra.ast.Expr.op(v_Expr_op_oe) => opExprLength(v_Expr_op_oe)
    case hydra.ast.Expr.brackets(v_Expr_brackets_be) => bracketExprLength(v_Expr_brackets_be)
    case hydra.ast.Expr.seq(v_Expr_seq_se) => seqExprLength(v_Expr_seq_se)
}

lazy val fullBlockStyle: hydra.ast.BlockStyle = hydra.ast.BlockStyle(Some(hydra.serialization.doubleSpace), true, true)

lazy val halfBlockStyle: hydra.ast.BlockStyle = hydra.ast.BlockStyle(Some(hydra.serialization.doubleSpace), true, false)

def ifx(op: hydra.ast.Op)(lhs: hydra.ast.Expr)(rhs: hydra.ast.Expr): hydra.ast.Expr = hydra.ast.Expr.op(hydra.ast.OpExpr(op,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   lhs, rhs))

def indent(v1: scala.Predef.String): scala.Predef.String = hydra.serialization.customIndent(hydra.serialization.doubleSpace)(v1)

def indentBlock(v1: Seq[hydra.ast.Expr]): hydra.ast.Expr = hydra.serialization.customIndentBlock(hydra.serialization.doubleSpace)(v1)

def indentSubsequentLines(idt: scala.Predef.String)(e: hydra.ast.Expr): hydra.ast.Expr =
  hydra.ast.Expr.indent(hydra.ast.IndentedExpression(hydra.ast.IndentStyle.subsequentLines(idt), e))

def infixWs(op: scala.Predef.String)(l: hydra.ast.Expr)(r: hydra.ast.Expr): hydra.ast.Expr = hydra.serialization.spaceSep(Seq(l,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.serialization.cst(op), r))

def infixWsList(op: scala.Predef.String)(opers: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  {
  lazy val opExpr: hydra.ast.Expr = hydra.serialization.cst(op)
  def foldFun(e: Seq[hydra.ast.Expr])(r: hydra.ast.Expr): Seq[hydra.ast.Expr] =
    hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ast.Expr](e))(Seq(r))(hydra.lib.lists.cons[hydra.ast.Expr](r)(hydra.lib.lists.cons[hydra.ast.Expr](opExpr)(e)))
  hydra.serialization.spaceSep(hydra.lib.lists.foldl[Seq[hydra.ast.Expr], hydra.ast.Expr](foldFun)(Seq())(hydra.lib.lists.reverse[hydra.ast.Expr](opers)))
}

lazy val inlineStyle: hydra.ast.BlockStyle = hydra.ast.BlockStyle(None, false, false)

def newlineSep(v1: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.serialization.sep(hydra.ast.Op(hydra.serialization.sym(""), hydra.ast.Padding(hydra.ast.Ws.none,
     hydra.ast.Ws.break), 0, hydra.ast.Associativity.none))(v1)

lazy val noPadding: hydra.ast.Padding = hydra.ast.Padding(hydra.ast.Ws.none, hydra.ast.Ws.none)

def noSep(v1: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.serialization.sep(hydra.ast.Op(hydra.serialization.sym(""), hydra.ast.Padding(hydra.ast.Ws.none,
     hydra.ast.Ws.none), 0, hydra.ast.Associativity.none))(v1)

def num(i: Int): hydra.ast.Expr = hydra.serialization.cst(hydra.lib.literals.showInt32(i))

def op(s: scala.Predef.String)(p: Int)(assoc: hydra.ast.Associativity): hydra.ast.Op =
  hydra.ast.Op(hydra.serialization.sym(s), hydra.ast.Padding(hydra.ast.Ws.space, hydra.ast.Ws.space), p, assoc)

def orOp(newlines: Boolean): hydra.ast.Op =
  hydra.ast.Op(hydra.serialization.sym("|"), hydra.ast.Padding(hydra.ast.Ws.space,
     hydra.lib.logic.ifElse[hydra.ast.Ws](newlines)(hydra.ast.Ws.break)(hydra.ast.Ws.space)),
     0, hydra.ast.Associativity.none)

def orSep(style: hydra.ast.BlockStyle)(l: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  {
  lazy val newlines: Boolean = (style.newlineBeforeContent)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ast.Expr](hydra.serialization.cst(""))((h: hydra.ast.Expr) =>
    hydra.lib.lists.foldl[hydra.ast.Expr, hydra.ast.Expr]((acc: hydra.ast.Expr) =>
    (el: hydra.ast.Expr) =>
    hydra.serialization.ifx(hydra.serialization.orOp(newlines))(acc)(el))(h)(hydra.lib.lists.drop[hydra.ast.Expr](1)(l)))(hydra.lib.lists.maybeHead[hydra.ast.Expr](l))
}

def parenList(newlines: Boolean)(els: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  {
  lazy val style: hydra.ast.BlockStyle = hydra.lib.logic.ifElse[hydra.ast.BlockStyle](hydra.lib.logic.and(newlines)(hydra.lib.equality.gt[Int](hydra.lib.lists.length[hydra.ast.Expr](els))(1)))(hydra.serialization.halfBlockStyle)(hydra.serialization.inlineStyle)
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ast.Expr](els))(hydra.serialization.cst("()"))(hydra.serialization.brackets(hydra.serialization.parentheses)(style)(hydra.serialization.commaSep(style)(els)))
}

def parens(v1: hydra.ast.Expr): hydra.ast.Expr =
  hydra.serialization.brackets(hydra.serialization.parentheses)(hydra.serialization.inlineStyle)(v1)

lazy val parentheses: hydra.ast.Brackets = hydra.ast.Brackets("(", ")")

def parenthesize(exp: hydra.ast.Expr): hydra.ast.Expr =
  {
  def assocLeft(a: hydra.ast.Associativity): Boolean =
    a match
    case hydra.ast.Associativity.right => false
    case _ => true
  def assocRight(a: hydra.ast.Associativity): Boolean =
    a match
    case hydra.ast.Associativity.left => false
    case _ => true
  exp match
    case hydra.ast.Expr.brackets(v_Expr_brackets_bracketExpr) => hydra.ast.Expr.brackets(hydra.ast.BracketExpr(v_Expr_brackets_bracketExpr.brackets,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.serialization.parenthesize(v_Expr_brackets_bracketExpr.enclosed), (v_Expr_brackets_bracketExpr.style)))
    case hydra.ast.Expr.const(v_Expr_const_ignored) => exp
    case hydra.ast.Expr.indent(v_Expr_indent_indentExpr) => hydra.ast.Expr.indent(hydra.ast.IndentedExpression(v_Expr_indent_indentExpr.style,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.serialization.parenthesize(v_Expr_indent_indentExpr.expr)))
    case hydra.ast.Expr.seq(v_Expr_seq_seqExpr) => hydra.ast.Expr.seq(hydra.ast.SeqExpr(v_Expr_seq_seqExpr.op,
       hydra.lib.lists.map[hydra.ast.Expr, hydra.ast.Expr](hydra.serialization.parenthesize)(v_Expr_seq_seqExpr.elements)))
    case hydra.ast.Expr.op(v_Expr_op_opExpr) => {
      lazy val op: hydra.ast.Op = (v_Expr_op_opExpr.op)
      {
        lazy val prec: Int = (op.precedence)
        {
          lazy val assoc: hydra.ast.Associativity = (op.associativity)
          {
            lazy val lhs: hydra.ast.Expr = (v_Expr_op_opExpr.lhs)
            {
              lazy val rhs: hydra.ast.Expr = (v_Expr_op_opExpr.rhs)
              {
                lazy val `lhs_`: hydra.ast.Expr = hydra.serialization.parenthesize(lhs)
                {
                  lazy val `rhs_`: hydra.ast.Expr = hydra.serialization.parenthesize(rhs)
                  {
                    lazy val lhs2: hydra.ast.Expr = `lhs_` match
                      case hydra.ast.Expr.op(v_Expr_op_lopExpr) => {
                        lazy val lop: hydra.ast.Op = (v_Expr_op_lopExpr.op)
                        {
                          lazy val lprec: Int = (lop.precedence)
                          {
                            lazy val lassoc: hydra.ast.Associativity = (lop.associativity)
                            {
                              lazy val comparison: hydra.util.Comparison = hydra.lib.equality.compare[Int](prec)(lprec)
                              comparison match
                                case hydra.util.Comparison.lessThan => `lhs_`
                                case hydra.util.Comparison.greaterThan => hydra.serialization.parens(`lhs_`)
                                case hydra.util.Comparison.equalTo => hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.logic.and(assocLeft(assoc))(assocLeft(lassoc)))(`lhs_`)(hydra.serialization.parens(`lhs_`))
                            }
                          }
                        }
                      }
                      case _ => `lhs_`
                    {
                      lazy val rhs2: hydra.ast.Expr = `rhs_` match
                        case hydra.ast.Expr.op(v_Expr_op_ropExpr) => {
                          lazy val rop: hydra.ast.Op = (v_Expr_op_ropExpr.op)
                          {
                            lazy val rprec: Int = (rop.precedence)
                            {
                              lazy val rassoc: hydra.ast.Associativity = (rop.associativity)
                              {
                                lazy val comparison: hydra.util.Comparison = hydra.lib.equality.compare[Int](prec)(rprec)
                                comparison match
                                  case hydra.util.Comparison.lessThan => `rhs_`
                                  case hydra.util.Comparison.greaterThan => hydra.serialization.parens(`rhs_`)
                                  case hydra.util.Comparison.equalTo => hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.logic.and(assocRight(assoc))(assocRight(rassoc)))(`rhs_`)(hydra.serialization.parens(`rhs_`))
                              }
                            }
                          }
                        }
                        case _ => `rhs_`
                      hydra.ast.Expr.op(hydra.ast.OpExpr(op, lhs2, rhs2))
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}

def prefix(p: scala.Predef.String)(expr: hydra.ast.Expr): hydra.ast.Expr =
  {
  lazy val preOp: hydra.ast.Op = hydra.ast.Op(hydra.serialization.sym(p), hydra.ast.Padding(hydra.ast.Ws.none,
     hydra.ast.Ws.none), 0, hydra.ast.Associativity.none)
  hydra.serialization.ifx(preOp)(hydra.serialization.cst(""))(expr)
}

def printExpr(e: hydra.ast.Expr): scala.Predef.String =
  {
  def pad(ws: hydra.ast.Ws): scala.Predef.String =
    ws match
    case hydra.ast.Ws.none => ""
    case hydra.ast.Ws.space => " "
    case hydra.ast.Ws.break => "\n"
    case hydra.ast.Ws.breakAndIndent(v_Ws_breakAndIndent_ignored) => "\n"
    case hydra.ast.Ws.doubleBreak => "\n\n"
  def idt(ws: hydra.ast.Ws)(s: scala.Predef.String): scala.Predef.String =
    ws match
    case hydra.ast.Ws.breakAndIndent(v_Ws_breakAndIndent_indentStr) => hydra.serialization.customIndent(v_Ws_breakAndIndent_indentStr)(s)
    case _ => s
  e match
    case hydra.ast.Expr.const(v_Expr_const_symbol) => v_Expr_const_symbol
    case hydra.ast.Expr.indent(v_Expr_indent_indentExpr) => {
      lazy val style: hydra.ast.IndentStyle = (v_Expr_indent_indentExpr.style)
      {
        lazy val expr: hydra.ast.Expr = (v_Expr_indent_indentExpr.expr)
        {
          lazy val lns: Seq[scala.Predef.String] = hydra.lib.strings.lines(hydra.serialization.printExpr(expr))
          {
            lazy val ilns: Seq[scala.Predef.String] = style match
              case hydra.ast.IndentStyle.allLines(v_IndentStyle_allLines_idt2) => hydra.lib.lists.map[scala.Predef.String,
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                 scala.Predef.String]((line: scala.Predef.String) => hydra.lib.strings.cat2(v_IndentStyle_allLines_idt2)(line))(lns)
              case hydra.ast.IndentStyle.subsequentLines(v_IndentStyle_subsequentLines_idt2) => hydra.lib.logic.ifElse[Seq[scala.Predef.String]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[scala.Predef.String](lns))(1))(lns)(hydra.lib.maybes.fromMaybe[Seq[scala.Predef.String]](lns)(hydra.lib.maybes.map[Tuple2[scala.Predef.String,
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                 Seq[scala.Predef.String]], Seq[scala.Predef.String]]((uc: Tuple2[scala.Predef.String,
                 Seq[scala.Predef.String]]) =>
                hydra.lib.lists.cons[scala.Predef.String](hydra.lib.pairs.first[scala.Predef.String,
                   Seq[scala.Predef.String]](uc))(hydra.lib.lists.map[scala.Predef.String,
                   scala.Predef.String]((line: scala.Predef.String) =>
                hydra.lib.strings.cat2(v_IndentStyle_subsequentLines_idt2)(line))(hydra.lib.pairs.second[scala.Predef.String,
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                   Seq[scala.Predef.String]](uc))))(hydra.lib.lists.uncons[scala.Predef.String](lns))))
            hydra.lib.strings.intercalate("\n")(ilns)
          }
        }
      }
    }
    case hydra.ast.Expr.seq(v_Expr_seq_seqExpr) => {
      lazy val sop: hydra.ast.Op = (v_Expr_seq_seqExpr.op)
      {
        lazy val ssym: scala.Predef.String = (sop.symbol)
        {
          lazy val spadding: hydra.ast.Padding = (sop.padding)
          {
            lazy val spadl: hydra.ast.Ws = (spadding.left)
            {
              lazy val spadr: hydra.ast.Ws = (spadding.right)
              {
                lazy val selements: Seq[hydra.ast.Expr] = (v_Expr_seq_seqExpr.elements)
                {
                  lazy val separator: scala.Predef.String = hydra.lib.strings.cat2(hydra.lib.strings.cat2(pad(spadl))(ssym))(pad(spadr))
                  {
                    lazy val printedElements: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.ast.Expr,
                       scala.Predef.String]((el: hydra.ast.Expr) => idt(spadr)(hydra.serialization.printExpr(el)))(selements)
                    hydra.lib.strings.intercalate(separator)(printedElements)
                  }
                }
              }
            }
          }
        }
      }
    }
    case hydra.ast.Expr.op(v_Expr_op_opExpr) => {
      lazy val op: hydra.ast.Op = (v_Expr_op_opExpr.op)
      {
        lazy val sym: scala.Predef.String = (op.symbol)
        {
          lazy val padding: hydra.ast.Padding = (op.padding)
          {
            lazy val padl: hydra.ast.Ws = (padding.left)
            {
              lazy val padr: hydra.ast.Ws = (padding.right)
              {
                lazy val l: hydra.ast.Expr = (v_Expr_op_opExpr.lhs)
                {
                  lazy val r: hydra.ast.Expr = (v_Expr_op_opExpr.rhs)
                  {
                    lazy val lhs: scala.Predef.String = idt(padl)(hydra.serialization.printExpr(l))
                    {
                      lazy val rhs: scala.Predef.String = idt(padr)(hydra.serialization.printExpr(r))
                      hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(lhs)(pad(padl)))(sym))(pad(padr)))(rhs)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    case hydra.ast.Expr.brackets(v_Expr_brackets_bracketExpr) => {
      lazy val brs: hydra.ast.Brackets = (v_Expr_brackets_bracketExpr.brackets)
      {
        lazy val l: scala.Predef.String = (brs.open)
        {
          lazy val r: scala.Predef.String = (brs.close)
          {
            lazy val e: hydra.ast.Expr = (v_Expr_brackets_bracketExpr.enclosed)
            {
              lazy val style: hydra.ast.BlockStyle = (v_Expr_brackets_bracketExpr.style)
              {
                lazy val body: scala.Predef.String = hydra.serialization.printExpr(e)
                {
                  lazy val doIndent: Option[scala.Predef.String] = (style.indent)
                  {
                    lazy val nlBefore: Boolean = (style.newlineBeforeContent)
                    {
                      lazy val nlAfter: Boolean = (style.newlineAfterContent)
                      {
                        lazy val ibody: scala.Predef.String = hydra.lib.maybes.maybe[scala.Predef.String,
                           scala.Predef.String](body)((idt2: scala.Predef.String) => hydra.serialization.customIndent(idt2)(body))(doIndent)
                        {
                          lazy val pre: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](nlBefore)("\n")("")
                          {
                            lazy val suf: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](nlAfter)("\n")("")
                            hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(l)(pre))(ibody))(suf))(r)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}

def semicolonSep(v1: Seq[hydra.ast.Expr]): hydra.ast.Expr = hydra.serialization.symbolSep(";")(hydra.serialization.inlineStyle)(v1)

def sep(op: hydra.ast.Op)(els: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ast.Expr](hydra.serialization.cst(""))((h: hydra.ast.Expr) =>
  hydra.lib.lists.foldl[hydra.ast.Expr, hydra.ast.Expr]((acc: hydra.ast.Expr) => (el: hydra.ast.Expr) => hydra.serialization.ifx(op)(acc)(el))(h)(hydra.lib.lists.drop[hydra.ast.Expr](1)(els)))(hydra.lib.lists.maybeHead[hydra.ast.Expr](els))

def spaceSep(v1: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.serialization.sep(hydra.ast.Op(hydra.serialization.sym(""), hydra.ast.Padding(hydra.ast.Ws.space,
     hydra.ast.Ws.none), 0, hydra.ast.Associativity.none))(v1)

lazy val squareBrackets: hydra.ast.Brackets = hydra.ast.Brackets("[", "]")

def structuralSep(op: hydra.ast.Op)(els: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ast.Expr](els))(hydra.serialization.cst(""))(hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ast.Expr](els))(1))(hydra.lib.maybes.fromMaybe[hydra.ast.Expr](hydra.serialization.cst(""))(hydra.lib.lists.maybeHead[hydra.ast.Expr](els)))(hydra.ast.Expr.seq(hydra.ast.SeqExpr(op,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     els))))

def structuralSpaceSep(v1: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  hydra.serialization.structuralSep(hydra.ast.Op(hydra.serialization.sym(""), hydra.ast.Padding(hydra.ast.Ws.space,
     hydra.ast.Ws.none), 0, hydra.ast.Associativity.none))(v1)

def suffix(s: scala.Predef.String)(expr: hydra.ast.Expr): hydra.ast.Expr =
  {
  lazy val sufOp: hydra.ast.Op = hydra.ast.Op(hydra.serialization.sym(s), hydra.ast.Padding(hydra.ast.Ws.none,
     hydra.ast.Ws.none), 0, hydra.ast.Associativity.none)
  hydra.serialization.ifx(sufOp)(expr)(hydra.serialization.cst(""))
}

def sym(s: scala.Predef.String): hydra.ast.Symbol = s

def symbolSep(symb: scala.Predef.String)(style: hydra.ast.BlockStyle)(l: Seq[hydra.ast.Expr]): hydra.ast.Expr =
  {
  lazy val breakCount: Int = hydra.lib.lists.length[Boolean](hydra.lib.lists.filter[Boolean]((`x_`: Boolean) => `x_`)(Seq(style.newlineBeforeContent,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     (style.newlineAfterContent))))
  lazy val break: hydra.ast.Ws = hydra.lib.logic.ifElse[hydra.ast.Ws](hydra.lib.equality.equal[Int](breakCount)(0))(hydra.ast.Ws.space)(hydra.lib.logic.ifElse[hydra.ast.Ws](hydra.lib.equality.equal[Int](breakCount)(1))(hydra.ast.Ws.break)(hydra.ast.Ws.doubleBreak))
  lazy val commaOp: hydra.ast.Op = hydra.ast.Op(hydra.serialization.sym(symb), hydra.ast.Padding(hydra.ast.Ws.none,
     break), 0, hydra.ast.Associativity.none)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ast.Expr](hydra.serialization.cst(""))((h: hydra.ast.Expr) =>
    hydra.lib.lists.foldl[hydra.ast.Expr, hydra.ast.Expr]((acc: hydra.ast.Expr) =>
    (el: hydra.ast.Expr) => hydra.serialization.ifx(commaOp)(acc)(el))(h)(hydra.lib.lists.drop[hydra.ast.Expr](1)(l)))(hydra.lib.lists.maybeHead[hydra.ast.Expr](l))
}

def tabIndent(e: hydra.ast.Expr): hydra.ast.Expr =
  hydra.ast.Expr.indent(hydra.ast.IndentedExpression(hydra.ast.IndentStyle.allLines("    "), e))

def tabIndentDoubleSpace(exprs: Seq[hydra.ast.Expr]): hydra.ast.Expr = hydra.serialization.tabIndent(hydra.serialization.doubleNewlineSep(exprs))

def tabIndentSingleSpace(exprs: Seq[hydra.ast.Expr]): hydra.ast.Expr = hydra.serialization.tabIndent(hydra.serialization.newlineSep(exprs))

def unsupportedType(label: scala.Predef.String): hydra.ast.Expr =
  hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.strings.cat2("[")(label))("]"))

def unsupportedVariant(label: scala.Predef.String)(obj: scala.Predef.String): hydra.ast.Expr =
  hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("[unsupported ")(label))(": "))(hydra.lib.literals.showString(obj)))("]"))

def withComma(e: hydra.ast.Expr): hydra.ast.Expr = hydra.serialization.noSep(Seq(e, hydra.serialization.cst(",")))

def withSemi(e: hydra.ast.Expr): hydra.ast.Expr = hydra.serialization.noSep(Seq(e, hydra.serialization.cst(";")))
