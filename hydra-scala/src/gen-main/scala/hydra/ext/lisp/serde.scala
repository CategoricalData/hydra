package hydra.ext.lisp.serde

import hydra.ext.lisp.syntax.*

def andExpressionToExpr(d: hydra.ext.lisp.syntax.Dialect)(andExpr: hydra.ext.lisp.syntax.AndExpression): hydra.ast.Expr =
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("and")))(hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(andExpr.expressions))))

def applicationToExpr(d: hydra.ext.lisp.syntax.Dialect)(app: hydra.ext.lisp.syntax.Application): hydra.ast.Expr =
  {
  lazy val funExpr: hydra.ext.lisp.syntax.Expression = (app.function)
  lazy val fun: hydra.ast.Expr = hydra.ext.lisp.serde.expressionToExpr(d)(funExpr)
  lazy val args: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression, hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(app.arguments)
  lazy val needsFuncall: Boolean = d match
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => funExpr match
      case hydra.ext.lisp.syntax.Expression.variable(v_Expression_variable_s) => false
      case _ => true
    case _ => false
  lazy val allParts: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](needsFuncall)(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("funcall"),
     fun))(args))(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(fun))(args))
  hydra.serialization.parens(hydra.serialization.spaceSep(allParts))
}

def caseExpressionToExpr(d: hydra.ext.lisp.syntax.Dialect)(caseExpr: hydra.ext.lisp.syntax.CaseExpression): hydra.ast.Expr =
  {
  lazy val scrutinee: hydra.ast.Expr = hydra.ext.lisp.serde.expressionToExpr(d)(caseExpr.scrutinee)
  lazy val clauses: Seq[hydra.ext.lisp.syntax.CaseClause] = (caseExpr.clauses)
  lazy val dflt: Option[hydra.ext.lisp.syntax.Expression] = (caseExpr.default)
  lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.CaseClause, hydra.ast.Expr]((c: hydra.ext.lisp.syntax.CaseClause) =>
    hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression,
       hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(c.keys))),
       hydra.ext.lisp.serde.expressionToExpr(d)(c.body)))))(clauses)
  lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.lisp.syntax.Expression](Seq())((e: hydra.ext.lisp.syntax.Expression) =>
    Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("else"), hydra.ext.lisp.serde.expressionToExpr(d)(e))))))(dflt)
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("case"),
     scrutinee), clauseExprs, defaultPart))))
}

def commentToExpr(c: hydra.ext.lisp.syntax.Comment): hydra.ast.Expr =
  {
  lazy val text: scala.Predef.String = (c.text)
  hydra.serialization.cst(hydra.lib.strings.cat2("; ")(text))
}

def condExpressionToExpr(d: hydra.ext.lisp.syntax.Dialect)(condExpr: hydra.ext.lisp.syntax.CondExpression): hydra.ast.Expr =
  {
  lazy val clauses: Seq[hydra.ext.lisp.syntax.CondClause] = (condExpr.clauses)
  lazy val dflt: Option[hydra.ext.lisp.syntax.Expression] = (condExpr.default)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => {
      lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[hydra.ext.lisp.syntax.CondClause,
         Seq[hydra.ast.Expr]]((c: hydra.ext.lisp.syntax.CondClause) =>
        Seq(hydra.ext.lisp.serde.expressionToExpr(d)(c.condition), hydra.ext.lisp.serde.expressionToExpr(d)(c.body)))(clauses))
      lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.lisp.syntax.Expression](Seq())((e: hydra.ext.lisp.syntax.Expression) =>
        Seq(hydra.serialization.cst(":else"), hydra.ext.lisp.serde.expressionToExpr(d)(e)))(dflt)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cond")),
         clauseExprs, defaultPart))))
    }
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => {
      lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.CondClause, hydra.ast.Expr]((c: hydra.ext.lisp.syntax.CondClause) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.ext.lisp.serde.expressionToExpr(d)(c.condition),
           hydra.ext.lisp.serde.expressionToExpr(d)(c.body)))))(clauses)
      lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.lisp.syntax.Expression](Seq())((e: hydra.ext.lisp.syntax.Expression) =>
        Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("t"), hydra.ext.lisp.serde.expressionToExpr(d)(e))))))(dflt)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cond")),
         clauseExprs, defaultPart))))
    }
    case hydra.ext.lisp.syntax.Dialect.commonLisp => {
      lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.CondClause, hydra.ast.Expr]((c: hydra.ext.lisp.syntax.CondClause) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.ext.lisp.serde.expressionToExpr(d)(c.condition),
           hydra.ext.lisp.serde.expressionToExpr(d)(c.body)))))(clauses)
      lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.lisp.syntax.Expression](Seq())((e: hydra.ext.lisp.syntax.Expression) =>
        Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("t"), hydra.ext.lisp.serde.expressionToExpr(d)(e))))))(dflt)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cond")),
         clauseExprs, defaultPart))))
    }
    case hydra.ext.lisp.syntax.Dialect.scheme => {
      lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.CondClause, hydra.ast.Expr]((c: hydra.ext.lisp.syntax.CondClause) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.ext.lisp.serde.expressionToExpr(d)(c.condition),
           hydra.ext.lisp.serde.expressionToExpr(d)(c.body)))))(clauses)
      lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.lisp.syntax.Expression](Seq())((e: hydra.ext.lisp.syntax.Expression) =>
        Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("else"), hydra.ext.lisp.serde.expressionToExpr(d)(e))))))(dflt)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cond")),
         clauseExprs, defaultPart))))
    }
}

def constantDefinitionToExpr(d: hydra.ext.lisp.syntax.Dialect)(cdef: hydra.ext.lisp.syntax.ConstantDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.ext.lisp.serde.symbolToExpr(cdef.name)
  lazy val value: hydra.ast.Expr = hydra.ext.lisp.serde.expressionToExpr(d)(cdef.value)
  hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(hydra.ext.lisp.serde.defconstKeyword(d)), name, value)))
}

def defKeyword(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "def"
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => "defvar"
  case hydra.ext.lisp.syntax.Dialect.commonLisp => "cl:defvar"
  case hydra.ext.lisp.syntax.Dialect.scheme => "define"

def defconstKeyword(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "def"
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => "defconst"
  case hydra.ext.lisp.syntax.Dialect.commonLisp => "cl:defconstant"
  case hydra.ext.lisp.syntax.Dialect.scheme => "define"

def defnKeyword(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "defn"
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => "defun"
  case hydra.ext.lisp.syntax.Dialect.commonLisp => "cl:defun"
  case hydra.ext.lisp.syntax.Dialect.scheme => "define"

def defrecordKeyword(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "defrecord"
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => "cl-defstruct"
  case hydra.ext.lisp.syntax.Dialect.commonLisp => "cl:defstruct"
  case hydra.ext.lisp.syntax.Dialect.scheme => "define-record-type"

def doExpressionToExpr(d: hydra.ext.lisp.syntax.Dialect)(doExpr: hydra.ext.lisp.syntax.DoExpression): hydra.ast.Expr =
  {
  lazy val kw: scala.Predef.String = d match
    case hydra.ext.lisp.syntax.Dialect.clojure => "do"
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => "progn"
    case hydra.ext.lisp.syntax.Dialect.commonLisp => "progn"
    case hydra.ext.lisp.syntax.Dialect.scheme => "begin"
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(kw)))(hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(doExpr.expressions))))
}

def docstringToExpr(ds: hydra.ext.lisp.syntax.Docstring): hydra.ast.Expr = hydra.serialization.cst(hydra.lib.strings.cat(Seq(";; ", ds)))

def exportDeclarationToExpr(d: hydra.ext.lisp.syntax.Dialect)(edecl: hydra.ext.lisp.syntax.ExportDeclaration): hydra.ast.Expr =
  {
  lazy val syms: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Symbol, hydra.ast.Expr](hydra.ext.lisp.serde.symbolToExpr)(edecl.symbols)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.cst("")
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ast.Expr, hydra.ast.Expr]((s: hydra.ast.Expr) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("provide"),
         hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), s))))))(syms))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(":export")))(hydra.lib.lists.map[hydra.ast.Expr,
       hydra.ast.Expr]((s: hydra.ast.Expr) =>
      hydra.serialization.noSep(Seq(hydra.serialization.cst(":"), s)))(syms))))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("export")))(syms)))
}

def expressionToExpr(d: hydra.ext.lisp.syntax.Dialect)(expr: hydra.ext.lisp.syntax.Expression): hydra.ast.Expr =
  expr match
  case hydra.ext.lisp.syntax.Expression.application(v_Expression_application_a) => hydra.ext.lisp.serde.applicationToExpr(d)(v_Expression_application_a)
  case hydra.ext.lisp.syntax.Expression.lambda(v_Expression_lambda_l) => hydra.ext.lisp.serde.lambdaToExpr(d)(v_Expression_lambda_l)
  case hydra.ext.lisp.syntax.Expression.let(v_Expression_let_l) => hydra.ext.lisp.serde.letExpressionToExpr(d)(v_Expression_let_l)
  case hydra.ext.lisp.syntax.Expression.`if`(v_Expression_if_i) => hydra.ext.lisp.serde.ifExpressionToExpr(d)(v_Expression_if_i)
  case hydra.ext.lisp.syntax.Expression.cond(v_Expression_cond_c) => hydra.ext.lisp.serde.condExpressionToExpr(d)(v_Expression_cond_c)
  case hydra.ext.lisp.syntax.Expression.`case`(v_Expression_case_c) => hydra.ext.lisp.serde.caseExpressionToExpr(d)(v_Expression_case_c)
  case hydra.ext.lisp.syntax.Expression.and(v_Expression_and_a) => hydra.ext.lisp.serde.andExpressionToExpr(d)(v_Expression_and_a)
  case hydra.ext.lisp.syntax.Expression.or(v_Expression_or_o) => hydra.ext.lisp.serde.orExpressionToExpr(d)(v_Expression_or_o)
  case hydra.ext.lisp.syntax.Expression.not(v_Expression_not_n) => hydra.ext.lisp.serde.notExpressionToExpr(d)(v_Expression_not_n)
  case hydra.ext.lisp.syntax.Expression.`do`(v_Expression_do_e) => hydra.ext.lisp.serde.doExpressionToExpr(d)(v_Expression_do_e)
  case hydra.ext.lisp.syntax.Expression.begin(v_Expression_begin_e) => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("begin")))(hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(v_Expression_begin_e.expressions))))
  case hydra.ext.lisp.syntax.Expression.variable(v_Expression_variable_v) => hydra.ext.lisp.serde.variableReferenceToExpr(d)(v_Expression_variable_v)
  case hydra.ext.lisp.syntax.Expression.literal(v_Expression_literal_l) => hydra.ext.lisp.serde.literalToExpr(d)(v_Expression_literal_l)
  case hydra.ext.lisp.syntax.Expression.list(v_Expression_list_l) => hydra.ext.lisp.serde.listLiteralToExpr(d)(v_Expression_list_l)
  case hydra.ext.lisp.syntax.Expression.vector(v_Expression_vector_v) => hydra.ext.lisp.serde.vectorLiteralToExpr(d)(v_Expression_vector_v)
  case hydra.ext.lisp.syntax.Expression.map(v_Expression_map_m) => hydra.ext.lisp.serde.mapLiteralToExpr(d)(v_Expression_map_m)
  case hydra.ext.lisp.syntax.Expression.set(v_Expression_set_s) => hydra.ext.lisp.serde.setLiteralToExpr(d)(v_Expression_set_s)
  case hydra.ext.lisp.syntax.Expression.cons(v_Expression_cons_c) => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("cons"),
     hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_cons_c.head), hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_cons_c.tail))))
  case hydra.ext.lisp.syntax.Expression.dottedPair(v_Expression_dottedPair_p) => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_dottedPair_p.car),
     hydra.serialization.cst("."), hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_dottedPair_p.cdr))))
  case hydra.ext.lisp.syntax.Expression.fieldAccess(v_Expression_fieldAccess_fa) => hydra.ext.lisp.serde.fieldAccessToExpr(d)(v_Expression_fieldAccess_fa)
  case hydra.ext.lisp.syntax.Expression.typeAnnotation(v_Expression_typeAnnotation_ta) => hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_typeAnnotation_ta.expression)
  case hydra.ext.lisp.syntax.Expression.quote(v_Expression_quote_q) => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
     hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_quote_q.body)))
  case hydra.ext.lisp.syntax.Expression.quasiquote(v_Expression_quasiquote_q) => hydra.serialization.noSep(Seq(hydra.serialization.cst("`"),
     hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_quasiquote_q.body)))
  case hydra.ext.lisp.syntax.Expression.unquote(v_Expression_unquote_u) => d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.noSep(Seq(hydra.serialization.cst("~"),
       hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_unquote_u.body)))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst(","),
       hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_unquote_u.body)))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst(","),
       hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_unquote_u.body)))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.noSep(Seq(hydra.serialization.cst(","),
       hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_unquote_u.body)))
  case hydra.ext.lisp.syntax.Expression.splicingUnquote(v_Expression_splicingUnquote_su) => d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.noSep(Seq(hydra.serialization.cst("~@"),
       hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_splicingUnquote_su.body)))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst(",@"),
       hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_splicingUnquote_su.body)))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst(",@"),
       hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_splicingUnquote_su.body)))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.noSep(Seq(hydra.serialization.cst(",@"),
       hydra.ext.lisp.serde.expressionToExpr(d)(v_Expression_splicingUnquote_su.body)))
  case hydra.ext.lisp.syntax.Expression.sExpression(v_Expression_sExpression_s) => hydra.ext.lisp.serde.sExpressionToExpr(v_Expression_sExpression_s)

def falseExpr(d: hydra.ext.lisp.syntax.Dialect): hydra.ast.Expr =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.cst("false")
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.cst("nil")
  case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.cst("cl:nil")
  case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.cst("#f")

def fieldAccessToExpr(d: hydra.ext.lisp.syntax.Dialect)(fa: hydra.ext.lisp.syntax.FieldAccess): hydra.ast.Expr =
  {
  lazy val rtype: hydra.ast.Expr = hydra.ext.lisp.serde.symbolToExpr(fa.recordType)
  lazy val field: hydra.ast.Expr = hydra.ext.lisp.serde.symbolToExpr(fa.field)
  lazy val target: hydra.ast.Expr = hydra.ext.lisp.serde.expressionToExpr(d)(fa.target)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(hydra.serialization.cst(":"),
       field)), target)))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(rtype,
       hydra.serialization.cst("-"), field)), target)))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(rtype,
       hydra.serialization.cst("-"), field)), target)))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(rtype,
       hydra.serialization.cst("-"), field)), target)))
}

def functionDefinitionToExpr(d: hydra.ext.lisp.syntax.Dialect)(fdef: hydra.ext.lisp.syntax.FunctionDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.ext.lisp.serde.symbolToExpr(fdef.name)
  lazy val params: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Symbol, hydra.ast.Expr](hydra.ext.lisp.serde.symbolToExpr)(fdef.params)
  lazy val body: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression, hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(fdef.body)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defn"),
       name), Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(params))),
       body))))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defun"),
       name), Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defun"),
       name), Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("define")),
       Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(name))(params)))),
       body))))
}

def ifExpressionToExpr(d: hydra.ext.lisp.syntax.Dialect)(ifExpr: hydra.ext.lisp.syntax.IfExpression): hydra.ast.Expr =
  {
  lazy val cond: hydra.ast.Expr = hydra.ext.lisp.serde.expressionToExpr(d)(ifExpr.condition)
  lazy val `then`: hydra.ast.Expr = hydra.ext.lisp.serde.expressionToExpr(d)(ifExpr.`then`)
  lazy val `else`: Option[hydra.ext.lisp.syntax.Expression] = (ifExpr.`else`)
  lazy val elsePart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.lisp.syntax.Expression](Seq())((e: hydra.ext.lisp.syntax.Expression) => Seq(hydra.ext.lisp.serde.expressionToExpr(d)(e)))(`else`)
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("if"),
     cond, `then`), elsePart))))
}

def importDeclarationToExpr(d: hydra.ext.lisp.syntax.Dialect)(idecl: hydra.ext.lisp.syntax.ImportDeclaration): hydra.ast.Expr =
  {
  lazy val modName: scala.Predef.String = (idecl.module)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(":require"),
       hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(modName)))))))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("require"),
       hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst(modName))))))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(":use"),
       hydra.serialization.cst(hydra.lib.strings.cat2(":")(modName)))))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("import"),
       hydra.serialization.parens(hydra.serialization.cst(modName)))))
}

def keywordToExpr(d: hydra.ext.lisp.syntax.Dialect)(k: hydra.ext.lisp.syntax.Keyword): hydra.ast.Expr =
  {
  lazy val name: scala.Predef.String = (k.name)
  lazy val ns: Option[scala.Predef.String] = (k.namespace)
  d match
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst(name)))
    case _ => hydra.serialization.cst(hydra.lib.maybes.maybe[scala.Predef.String, scala.Predef.String](hydra.lib.strings.cat2(":")(name))((n: scala.Predef.String) => hydra.lib.strings.cat(Seq(n,
       "/:", name)))(ns))
}

def lambdaKeyword(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "fn"
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => "lambda"
  case hydra.ext.lisp.syntax.Dialect.commonLisp => "cl:lambda"
  case hydra.ext.lisp.syntax.Dialect.scheme => "lambda"

def lambdaToExpr(d: hydra.ext.lisp.syntax.Dialect)(lam: hydra.ext.lisp.syntax.Lambda): hydra.ast.Expr =
  {
  lazy val params: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Symbol, hydra.ast.Expr](hydra.ext.lisp.serde.symbolToExpr)(lam.params)
  lazy val body: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression, hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(lam.body)
  lazy val mname: Option[hydra.ext.lisp.syntax.Symbol] = (lam.name)
  lazy val kw: scala.Predef.String = hydra.ext.lisp.serde.lambdaKeyword(d)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.lisp.syntax.Symbol](hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
       Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(params))),
       body)))))((sym: hydra.ext.lisp.syntax.Symbol) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw),
         hydra.ext.lisp.serde.symbolToExpr(sym)), Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(params))),
         body)))))(mname)
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
       Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
       Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
       Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
}

def letExpressionToExpr(d: hydra.ext.lisp.syntax.Dialect)(letExpr: hydra.ext.lisp.syntax.LetExpression): hydra.ast.Expr =
  {
  lazy val kind: hydra.ext.lisp.syntax.LetKind = (letExpr.kind)
  lazy val bindings: Seq[hydra.ext.lisp.syntax.LetBinding] = (letExpr.bindings)
  lazy val body: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression, hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(letExpr.body)
  lazy val bindingPairs: Seq[Tuple2[hydra.ast.Expr, hydra.ast.Expr]] = hydra.lib.lists.map[hydra.ext.lisp.syntax.LetBinding,
     Tuple2[hydra.ast.Expr, hydra.ast.Expr]]((b: hydra.ext.lisp.syntax.LetBinding) =>
    b match
    case hydra.ext.lisp.syntax.LetBinding.simple(v_LetBinding_simple_sb) => Tuple2(hydra.ext.lisp.serde.symbolToExpr(v_LetBinding_simple_sb.name),
       hydra.ext.lisp.serde.expressionToExpr(d)(v_LetBinding_simple_sb.value))
    case hydra.ext.lisp.syntax.LetBinding.destructuring(v_LetBinding_destructuring__) => Tuple2(hydra.serialization.cst("<destructuring>"),
       hydra.serialization.cst("<destructuring>")))(bindings)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => kind match
      case hydra.ext.lisp.syntax.LetKind.recursive => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("let")),
         Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[Tuple2[hydra.ast.Expr,
         hydra.ast.Expr], Seq[hydra.ast.Expr]]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        Seq(hydra.lib.pairs.first[hydra.ast.Expr, hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr, hydra.ast.Expr](p)))(bindingPairs))))), body))))
      case hydra.ext.lisp.syntax.LetKind.parallel => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("let")),
         Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[Tuple2[hydra.ast.Expr,
         hydra.ast.Expr], Seq[hydra.ast.Expr]]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        Seq(hydra.lib.pairs.first[hydra.ast.Expr, hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr, hydra.ast.Expr](p)))(bindingPairs))))), body))))
      case hydra.ext.lisp.syntax.LetKind.sequential => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("let")),
         Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[Tuple2[hydra.ast.Expr,
         hydra.ast.Expr], Seq[hydra.ast.Expr]]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        Seq(hydra.lib.pairs.first[hydra.ast.Expr, hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr, hydra.ast.Expr](p)))(bindingPairs))))), body))))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => {
      lazy val kw: scala.Predef.String = kind match
        case hydra.ext.lisp.syntax.LetKind.parallel => "let"
        case hydra.ext.lisp.syntax.LetKind.sequential => "let*"
        case hydra.ext.lisp.syntax.LetKind.recursive => "letrec"
      lazy val bindingExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[Tuple2[hydra.ast.Expr, hydra.ast.Expr],
         hydra.ast.Expr]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lib.pairs.first[hydra.ast.Expr,
           hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr, hydra.ast.Expr](p)))))(bindingPairs)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
         Seq(hydra.serialization.parens(hydra.serialization.spaceSep(bindingExprs))), body))))
    }
    case hydra.ext.lisp.syntax.Dialect.commonLisp => {
      lazy val kw: scala.Predef.String = kind match
        case hydra.ext.lisp.syntax.LetKind.parallel => "let"
        case hydra.ext.lisp.syntax.LetKind.sequential => "let*"
        case hydra.ext.lisp.syntax.LetKind.recursive => "letrec"
      lazy val bindingExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[Tuple2[hydra.ast.Expr, hydra.ast.Expr],
         hydra.ast.Expr]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lib.pairs.first[hydra.ast.Expr,
           hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr, hydra.ast.Expr](p)))))(bindingPairs)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
         Seq(hydra.serialization.parens(hydra.serialization.spaceSep(bindingExprs))), body))))
    }
    case hydra.ext.lisp.syntax.Dialect.scheme => {
      lazy val kw: scala.Predef.String = kind match
        case hydra.ext.lisp.syntax.LetKind.parallel => "let"
        case hydra.ext.lisp.syntax.LetKind.sequential => "let*"
        case hydra.ext.lisp.syntax.LetKind.recursive => "letrec"
      lazy val bindingExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[Tuple2[hydra.ast.Expr, hydra.ast.Expr],
         hydra.ast.Expr]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lib.pairs.first[hydra.ast.Expr,
           hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr, hydra.ast.Expr](p)))))(bindingPairs)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
         Seq(hydra.serialization.parens(hydra.serialization.spaceSep(bindingExprs))), body))))
    }
}

def listKeyword(d: hydra.ext.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => "list"
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => "list"
  case hydra.ext.lisp.syntax.Dialect.commonLisp => "cl:list"
  case hydra.ext.lisp.syntax.Dialect.scheme => "list"

def listLiteralToExpr(d: hydra.ext.lisp.syntax.Dialect)(ll: hydra.ext.lisp.syntax.ListLiteral): hydra.ast.Expr =
  {
  lazy val elems: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression, hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(ll.elements)
  lazy val quoted: Boolean = (ll.quoted)
  hydra.lib.logic.ifElse[hydra.ast.Expr](quoted)(hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
     hydra.serialization.parens(hydra.serialization.spaceSep(elems)))))(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(hydra.ext.lisp.serde.listKeyword(d))))(elems))))
}

def literalToExpr(d: hydra.ext.lisp.syntax.Dialect)(lit: hydra.ext.lisp.syntax.Literal): hydra.ast.Expr =
  lit match
  case hydra.ext.lisp.syntax.Literal.integer(v_Literal_integer_i) => hydra.serialization.cst(hydra.lib.literals.showBigint(v_Literal_integer_i.value))
  case hydra.ext.lisp.syntax.Literal.float(v_Literal_float_f) => hydra.serialization.cst(hydra.lib.literals.showBigfloat(v_Literal_float_f.value))
  case hydra.ext.lisp.syntax.Literal.string(v_Literal_string_s) => {
    lazy val e1: scala.Predef.String = hydra.lib.strings.intercalate("\\\\")(hydra.lib.strings.splitOn("\\")(v_Literal_string_s))
    d match
      case hydra.ext.lisp.syntax.Dialect.commonLisp => {
        lazy val escaped: scala.Predef.String = hydra.lib.strings.intercalate("\\\"")(hydra.lib.strings.splitOn("\"")(e1))
        hydra.serialization.cst(hydra.lib.strings.cat(Seq("\"", escaped, "\"")))
      }
      case hydra.ext.lisp.syntax.Dialect.clojure => {
        lazy val e2: scala.Predef.String = hydra.lib.strings.intercalate("\\n")(hydra.lib.strings.splitOn(hydra.lib.strings.fromList(Seq(10)))(e1))
        {
          lazy val e3: scala.Predef.String = hydra.lib.strings.intercalate("\\r")(hydra.lib.strings.splitOn(hydra.lib.strings.fromList(Seq(13)))(e2))
          {
            lazy val e4: scala.Predef.String = hydra.lib.strings.intercalate("\\t")(hydra.lib.strings.splitOn(hydra.lib.strings.fromList(Seq(9)))(e3))
            {
              lazy val escaped: scala.Predef.String = hydra.lib.strings.intercalate("\\\"")(hydra.lib.strings.splitOn("\"")(e4))
              hydra.serialization.cst(hydra.lib.strings.cat(Seq("\"", escaped, "\"")))
            }
          }
        }
      }
      case hydra.ext.lisp.syntax.Dialect.emacsLisp => {
        lazy val e2: scala.Predef.String = hydra.lib.strings.intercalate("\\n")(hydra.lib.strings.splitOn(hydra.lib.strings.fromList(Seq(10)))(e1))
        {
          lazy val e3: scala.Predef.String = hydra.lib.strings.intercalate("\\r")(hydra.lib.strings.splitOn(hydra.lib.strings.fromList(Seq(13)))(e2))
          {
            lazy val e4: scala.Predef.String = hydra.lib.strings.intercalate("\\t")(hydra.lib.strings.splitOn(hydra.lib.strings.fromList(Seq(9)))(e3))
            {
              lazy val escaped: scala.Predef.String = hydra.lib.strings.intercalate("\\\"")(hydra.lib.strings.splitOn("\"")(e4))
              hydra.serialization.cst(hydra.lib.strings.cat(Seq("\"", escaped, "\"")))
            }
          }
        }
      }
      case hydra.ext.lisp.syntax.Dialect.scheme => {
        lazy val e2: scala.Predef.String = hydra.lib.strings.intercalate("\\n")(hydra.lib.strings.splitOn(hydra.lib.strings.fromList(Seq(10)))(e1))
        {
          lazy val e3: scala.Predef.String = hydra.lib.strings.intercalate("\\r")(hydra.lib.strings.splitOn(hydra.lib.strings.fromList(Seq(13)))(e2))
          {
            lazy val e4: scala.Predef.String = hydra.lib.strings.intercalate("\\t")(hydra.lib.strings.splitOn(hydra.lib.strings.fromList(Seq(9)))(e3))
            {
              lazy val escaped: scala.Predef.String = hydra.lib.strings.intercalate("\\\"")(hydra.lib.strings.splitOn("\"")(e4))
              hydra.serialization.cst(hydra.lib.strings.cat(Seq("\"", escaped, "\"")))
            }
          }
        }
      }
  }
  case hydra.ext.lisp.syntax.Literal.character(v_Literal_character_c) => {
    lazy val ch: scala.Predef.String = (v_Literal_character_c.value)
    d match
      case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.cst(hydra.lib.strings.cat2("\\")(ch))
      case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.cst(hydra.lib.strings.cat2("?")(ch))
      case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.cst(hydra.lib.strings.cat2("#\\")(ch))
      case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.cst(hydra.lib.strings.cat2("#\\")(ch))
  }
  case hydra.ext.lisp.syntax.Literal.boolean(v_Literal_boolean_b) => hydra.lib.logic.ifElse[hydra.ast.Expr](v_Literal_boolean_b)(hydra.ext.lisp.serde.trueExpr(d))(hydra.ext.lisp.serde.falseExpr(d))
  case hydra.ext.lisp.syntax.Literal.nil => hydra.ext.lisp.serde.nilExpr(d)
  case hydra.ext.lisp.syntax.Literal.keyword(v_Literal_keyword_k) => hydra.ext.lisp.serde.keywordToExpr(d)(v_Literal_keyword_k)
  case hydra.ext.lisp.syntax.Literal.symbol(v_Literal_symbol_s) => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
     hydra.ext.lisp.serde.symbolToExpr(v_Literal_symbol_s)))

def macroDefinitionToExpr(d: hydra.ext.lisp.syntax.Dialect)(mdef: hydra.ext.lisp.syntax.MacroDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.ext.lisp.serde.symbolToExpr(mdef.name)
  lazy val params: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Symbol, hydra.ast.Expr](hydra.ext.lisp.serde.symbolToExpr)(mdef.params)
  lazy val body: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression, hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(mdef.body)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defmacro"),
       name), Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(params))),
       body))))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defmacro"),
       name), Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defmacro"),
       name), Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("define-syntax"),
       name), body))))
}

def mapLiteralToExpr(d: hydra.ext.lisp.syntax.Dialect)(ml: hydra.ext.lisp.syntax.MapLiteral): hydra.ast.Expr =
  {
  lazy val entries: Seq[hydra.ext.lisp.syntax.MapEntry] = (ml.entries)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.brackets(hydra.serialization.curlyBraces)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[hydra.ext.lisp.syntax.MapEntry,
       Seq[hydra.ast.Expr]]((e: hydra.ext.lisp.syntax.MapEntry) =>
      Seq(hydra.ext.lisp.serde.expressionToExpr(d)(e.key), hydra.ext.lisp.serde.expressionToExpr(d)(e.value)))(entries))))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
       hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.lisp.syntax.MapEntry,
       hydra.ast.Expr]((e: hydra.ext.lisp.syntax.MapEntry) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.ext.lisp.serde.expressionToExpr(d)(e.key),
         hydra.serialization.cst("."), hydra.ext.lisp.serde.expressionToExpr(d)(e.value)))))(entries)))))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
       hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.lisp.syntax.MapEntry,
       hydra.ast.Expr]((e: hydra.ext.lisp.syntax.MapEntry) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.ext.lisp.serde.expressionToExpr(d)(e.key),
         hydra.serialization.cst("."), hydra.ext.lisp.serde.expressionToExpr(d)(e.value)))))(entries)))))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("list")))(hydra.lib.lists.map[hydra.ext.lisp.syntax.MapEntry,
       hydra.ast.Expr]((e: hydra.ext.lisp.syntax.MapEntry) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("cons"), hydra.ext.lisp.serde.expressionToExpr(d)(e.key),
         hydra.ext.lisp.serde.expressionToExpr(d)(e.value)))))(entries))))
}

def moduleDeclarationToExpr(d: hydra.ext.lisp.syntax.Dialect)(mdecl: hydra.ext.lisp.syntax.ModuleDeclaration): hydra.ast.Expr =
  {
  lazy val name: scala.Predef.String = (mdecl.name)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("ns"),
       hydra.serialization.cst(name))))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.newlineSep(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("require"),
       hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst("cl-lib")))))),
       hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("provide"),
       hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst(name))))))))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.newlineSep(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("defpackage"),
       hydra.serialization.cst(hydra.lib.strings.cat2(":")(name))))), hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("in-package"),
       hydra.serialization.cst(hydra.lib.strings.cat2(":")(name)))))))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("define-library"),
       hydra.serialization.parens(hydra.serialization.cst(name)))))
}

def nilExpr(d: hydra.ext.lisp.syntax.Dialect): hydra.ast.Expr =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.cst("nil")
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.cst("nil")
  case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.cst("cl:nil")
  case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.cst("'()")

def notExpressionToExpr(d: hydra.ext.lisp.syntax.Dialect)(notExpr: hydra.ext.lisp.syntax.NotExpression): hydra.ast.Expr =
  hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("not"), hydra.ext.lisp.serde.expressionToExpr(d)(notExpr.expression))))

def orExpressionToExpr(d: hydra.ext.lisp.syntax.Dialect)(orExpr: hydra.ext.lisp.syntax.OrExpression): hydra.ast.Expr =
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("or")))(hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(orExpr.expressions))))

def programToExpr(prog: hydra.ext.lisp.syntax.Program): hydra.ast.Expr =
  {
  lazy val d: hydra.ext.lisp.syntax.Dialect = (prog.dialect)
  lazy val modDecl: Option[hydra.ext.lisp.syntax.ModuleDeclaration] = (prog.module)
  lazy val imports: Seq[hydra.ext.lisp.syntax.ImportDeclaration] = (prog.imports)
  lazy val exports: Seq[hydra.ext.lisp.syntax.ExportDeclaration] = (prog.exports)
  lazy val forms: Seq[hydra.ext.lisp.syntax.TopLevelFormWithComments] = (prog.forms)
  lazy val formPart: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.TopLevelFormWithComments,
     hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.TopLevelFormWithComments) => hydra.ext.lisp.serde.topLevelFormWithCommentsToExpr(d)(v1))(forms)
  lazy val importNames: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.ext.lisp.syntax.ImportDeclaration,
     scala.Predef.String]((idecl: hydra.ext.lisp.syntax.ImportDeclaration) => (idecl.module))(imports)
  lazy val exportSyms: Seq[hydra.ast.Expr] = hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[hydra.ext.lisp.syntax.ExportDeclaration,
     Seq[hydra.ast.Expr]]((edecl: hydra.ext.lisp.syntax.ExportDeclaration) =>
    hydra.lib.lists.map[hydra.ext.lisp.syntax.Symbol, hydra.ast.Expr](hydra.ext.lisp.serde.symbolToExpr)(edecl.symbols))(exports))
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.lisp.syntax.ModuleDeclaration](hydra.serialization.doubleNewlineSep(formPart))((m: hydra.ext.lisp.syntax.ModuleDeclaration) =>
      {
      lazy val nameStr: scala.Predef.String = (m.name)
      lazy val requireClauses: Seq[hydra.ast.Expr] = hydra.lib.lists.map[scala.Predef.String, hydra.ast.Expr]((imp: scala.Predef.String) =>
        hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(imp),
           hydra.serialization.cst(":refer"), hydra.serialization.cst(":all")))))(importNames)
      lazy val nsForm: hydra.ast.Expr = hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ast.Expr](requireClauses))(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("ns"),
         hydra.serialization.cst(nameStr)))))(hydra.serialization.parens(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("ns"),
         hydra.serialization.cst(nameStr))), hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("  (:require")))(requireClauses)),
         hydra.serialization.cst(")")))))
      {
        lazy val varNames: Seq[hydra.ast.Expr] = hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[hydra.ext.lisp.syntax.TopLevelFormWithComments,
           Seq[hydra.ast.Expr]]((fwc: hydra.ext.lisp.syntax.TopLevelFormWithComments) =>
          {
          lazy val form: hydra.ext.lisp.syntax.TopLevelForm = (fwc.form)
          form match
            case hydra.ext.lisp.syntax.TopLevelForm.variable(v_TopLevelForm_variable_vd) => Seq(hydra.ext.lisp.serde.symbolToExpr(v_TopLevelForm_variable_vd.name))
            case hydra.ext.lisp.syntax.TopLevelForm.function(v_TopLevelForm_function_fd) => Seq(hydra.ext.lisp.serde.symbolToExpr(v_TopLevelForm_function_fd.name))
            case _ => Seq()
        })(forms))
        {
          lazy val declareForm: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ast.Expr](varNames))(Seq())(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("declare")))(varNames)))))
          hydra.serialization.doubleNewlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(nsForm), declareForm, formPart)))
        }
      }
    })(modDecl)
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.lisp.syntax.ModuleDeclaration](hydra.serialization.doubleNewlineSep(formPart))((m: hydra.ext.lisp.syntax.ModuleDeclaration) =>
      {
      lazy val nameStr: scala.Predef.String = (m.name)
      lazy val requireClLib: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("require"),
         hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst("cl-lib"))))))
      lazy val requireImports: Seq[hydra.ast.Expr] = hydra.lib.lists.map[scala.Predef.String, hydra.ast.Expr]((imp: scala.Predef.String) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("require"),
           hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst(imp)))))))(importNames)
      lazy val provideForm: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("provide"),
         hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst(nameStr))))))
      hydra.serialization.doubleNewlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(requireClLib), requireImports, formPart, Seq(provideForm))))
    })(modDecl)
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.lisp.syntax.ModuleDeclaration](hydra.serialization.doubleNewlineSep(formPart))((m: hydra.ext.lisp.syntax.ModuleDeclaration) =>
      {
      lazy val nameStr: scala.Predef.String = (m.name)
      lazy val colonName: scala.Predef.String = hydra.lib.strings.cat2(":")(nameStr)
      lazy val useClause: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(":use"),
         hydra.serialization.cst(":cl")))(hydra.lib.lists.map[scala.Predef.String, hydra.ast.Expr]((imp: scala.Predef.String) => hydra.serialization.cst(hydra.lib.strings.cat2(":")(imp)))(importNames))))
      lazy val exportClause: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ast.Expr](exportSyms))(Seq())(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(":export")))(hydra.lib.lists.map[hydra.ast.Expr,
         hydra.ast.Expr]((s: hydra.ast.Expr) =>
        hydra.serialization.noSep(Seq(hydra.serialization.cst(":"), s)))(exportSyms))))))
      lazy val defpkgForm: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.newlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("defpackage"),
         hydra.serialization.cst(colonName)))), Seq(useClause), exportClause))))
      lazy val inpkgForm: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("in-package"),
         hydra.serialization.cst(colonName))))
      hydra.serialization.doubleNewlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(defpkgForm, inpkgForm), formPart)))
    })(modDecl)
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.lisp.syntax.ModuleDeclaration](hydra.serialization.doubleNewlineSep(formPart))((m: hydra.ext.lisp.syntax.ModuleDeclaration) =>
      {
      lazy val nameStr: scala.Predef.String = (m.name)
      lazy val nameParts: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((p: scala.Predef.String) => hydra.formatting.convertCaseCamelToLowerSnake(p))(hydra.lib.strings.splitOn(".")(nameStr))
      lazy val nameExpr: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((p: scala.Predef.String) => hydra.serialization.cst(p))(nameParts)))
      lazy val domainImportExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.ImportDeclaration,
         hydra.ast.Expr]((idecl: hydra.ext.lisp.syntax.ImportDeclaration) =>
        {
        lazy val nsName: scala.Predef.String = (idecl.module)
        lazy val nsParts: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((p: scala.Predef.String) => hydra.formatting.convertCaseCamelToLowerSnake(p))(hydra.lib.strings.splitOn(".")(nsName))
        hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[scala.Predef.String,
           hydra.ast.Expr]((p: scala.Predef.String) => hydra.serialization.cst(p))(nsParts)))
      })(imports)
      lazy val schemeBaseExpr: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("scheme"),
         hydra.serialization.cst("base"))))
      lazy val allImportExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.concat2[hydra.ast.Expr](Seq(schemeBaseExpr))(domainImportExprs)
      lazy val importClause: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("import")))(allImportExprs)))
      lazy val exportClauses: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.ExportDeclaration,
         hydra.ast.Expr]((edecl: hydra.ext.lisp.syntax.ExportDeclaration) => hydra.ext.lisp.serde.exportDeclarationToExpr(d)(edecl))(exports)
      lazy val beginClause: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.newlineSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("begin")))(formPart)))
      hydra.serialization.parens(hydra.serialization.newlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("define-library"),
         nameExpr))), exportClauses, Seq(importClause), Seq(beginClause)))))
    })(modDecl)
}

def recordTypeDefinitionToExpr(d: hydra.ext.lisp.syntax.Dialect)(rdef: hydra.ext.lisp.syntax.RecordTypeDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.ext.lisp.serde.symbolToExpr(rdef.name)
  lazy val fields: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.FieldDefinition, hydra.ast.Expr]((f: hydra.ext.lisp.syntax.FieldDefinition) => hydra.ext.lisp.serde.symbolToExpr(f.name))(rdef.fields)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => {
      lazy val nameStr: scala.Predef.String = (rdef.name)
      lazy val fieldNames: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.ext.lisp.syntax.FieldDefinition,
         scala.Predef.String]((f: hydra.ext.lisp.syntax.FieldDefinition) => (f.name))(rdef.fields)
      lazy val defrecordForm: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("defrecord"),
         name, hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(fields)))))
      lazy val makeAlias: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defn"),
         hydra.serialization.cst(hydra.lib.strings.cat2("make-")(nameStr))), Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(fields))),
         Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(hydra.lib.strings.cat2("->")(nameStr))))(hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((fn: scala.Predef.String) => hydra.serialization.cst(fn))(fieldNames)))))))))
      hydra.serialization.newlineSep(Seq(defrecordForm, makeAlias))
    }
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cl-defstruct"),
       name), fields))))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cl:defstruct"),
       name), fields))))
    case hydra.ext.lisp.syntax.Dialect.scheme => {
      lazy val nameStr: scala.Predef.String = (rdef.name)
      lazy val fieldNames: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.ext.lisp.syntax.FieldDefinition,
         scala.Predef.String]((f: hydra.ext.lisp.syntax.FieldDefinition) => (f.name))(rdef.fields)
      lazy val constructor: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(hydra.lib.strings.cat2("make-")(nameStr))))(hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((fn: scala.Predef.String) => hydra.serialization.cst(fn))(fieldNames))))
      lazy val predicate: hydra.ast.Expr = hydra.serialization.cst(hydra.lib.strings.cat2(nameStr)("?"))
      lazy val accessors: Seq[hydra.ast.Expr] = hydra.lib.lists.map[scala.Predef.String, hydra.ast.Expr]((fn: scala.Predef.String) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(fn), hydra.serialization.cst(hydra.lib.strings.cat(Seq(nameStr,
           "-", fn)))))))(fieldNames)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("define-record-type"),
         name, constructor, predicate), accessors))))
    }
}

def sExpressionToExpr(sexpr: hydra.ext.lisp.syntax.SExpression): hydra.ast.Expr =
  sexpr match
  case hydra.ext.lisp.syntax.SExpression.atom(v_SExpression_atom_a) => hydra.serialization.cst(v_SExpression_atom_a)
  case hydra.ext.lisp.syntax.SExpression.list(v_SExpression_list_elems) => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.lisp.syntax.SExpression,
     hydra.ast.Expr](hydra.ext.lisp.serde.sExpressionToExpr)(v_SExpression_list_elems)))

def setLiteralToExpr(d: hydra.ext.lisp.syntax.Dialect)(sl: hydra.ext.lisp.syntax.SetLiteral): hydra.ast.Expr =
  {
  lazy val elems: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression, hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(sl.elements)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.noSep(Seq(hydra.serialization.cst("#"),
       hydra.serialization.brackets(hydra.serialization.curlyBraces)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(elems))))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("list")))(elems)))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("cl:list")))(elems)))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("list")))(elems)))
}

def symbolToExpr(s: hydra.ext.lisp.syntax.Symbol): hydra.ast.Expr = hydra.serialization.cst(s)

def topLevelFormToExpr(d: hydra.ext.lisp.syntax.Dialect)(form: hydra.ext.lisp.syntax.TopLevelForm): hydra.ast.Expr =
  form match
  case hydra.ext.lisp.syntax.TopLevelForm.function(v_TopLevelForm_function_f) => hydra.ext.lisp.serde.functionDefinitionToExpr(d)(v_TopLevelForm_function_f)
  case hydra.ext.lisp.syntax.TopLevelForm.variable(v_TopLevelForm_variable_v) => hydra.ext.lisp.serde.variableDefinitionToExpr(d)(v_TopLevelForm_variable_v)
  case hydra.ext.lisp.syntax.TopLevelForm.constant(v_TopLevelForm_constant_c) => hydra.ext.lisp.serde.constantDefinitionToExpr(d)(v_TopLevelForm_constant_c)
  case hydra.ext.lisp.syntax.TopLevelForm.recordType(v_TopLevelForm_recordType_r) => hydra.ext.lisp.serde.recordTypeDefinitionToExpr(d)(v_TopLevelForm_recordType_r)
  case hydra.ext.lisp.syntax.TopLevelForm.`macro`(v_TopLevelForm_macro_m) => hydra.ext.lisp.serde.macroDefinitionToExpr(d)(v_TopLevelForm_macro_m)
  case hydra.ext.lisp.syntax.TopLevelForm.expression(v_TopLevelForm_expression_e) => hydra.ext.lisp.serde.expressionToExpr(d)(v_TopLevelForm_expression_e)

def topLevelFormWithCommentsToExpr(d: hydra.ext.lisp.syntax.Dialect)(fwc: hydra.ext.lisp.syntax.TopLevelFormWithComments): hydra.ast.Expr =
  {
  lazy val mdoc: Option[hydra.ext.lisp.syntax.Docstring] = (fwc.doc)
  lazy val mcomment: Option[hydra.ext.lisp.syntax.Comment] = (fwc.comment)
  lazy val form: hydra.ext.lisp.syntax.TopLevelForm = (fwc.form)
  lazy val docPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.lisp.syntax.Docstring](Seq())((ds: hydra.ext.lisp.syntax.Docstring) => Seq(hydra.ext.lisp.serde.docstringToExpr(ds)))(mdoc)
  lazy val commentPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.lisp.syntax.Comment](Seq())((c: hydra.ext.lisp.syntax.Comment) => Seq(hydra.ext.lisp.serde.commentToExpr(c)))(mcomment)
  lazy val formExpr: hydra.ast.Expr = hydra.ext.lisp.serde.topLevelFormToExpr(d)(form)
  hydra.serialization.newlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(commentPart, docPart, Seq(formExpr))))
}

def trueExpr(d: hydra.ext.lisp.syntax.Dialect): hydra.ast.Expr =
  d match
  case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.cst("true")
  case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.cst("t")
  case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.cst("cl:t")
  case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.cst("#t")

def variableDefinitionToExpr(d: hydra.ext.lisp.syntax.Dialect)(vdef: hydra.ext.lisp.syntax.VariableDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.ext.lisp.serde.symbolToExpr(vdef.name)
  lazy val value: hydra.ast.Expr = hydra.ext.lisp.serde.expressionToExpr(d)(vdef.value)
  hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(hydra.ext.lisp.serde.defKeyword(d)), name, value)))
}

def variableReferenceToExpr(d: hydra.ext.lisp.syntax.Dialect)(vref: hydra.ext.lisp.syntax.VariableReference): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.ext.lisp.serde.symbolToExpr(vref.name)
  lazy val isFnNs: Boolean = (vref.functionNamespace)
  hydra.lib.logic.ifElse[hydra.ast.Expr](isFnNs)(d match
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst("#'"), name))
    case hydra.ext.lisp.syntax.Dialect.clojure => name
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => name
    case hydra.ext.lisp.syntax.Dialect.scheme => name)(name)
}

def vectorLiteralToExpr(d: hydra.ext.lisp.syntax.Dialect)(vl: hydra.ext.lisp.syntax.VectorLiteral): hydra.ast.Expr =
  {
  lazy val elems: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.lisp.syntax.Expression, hydra.ast.Expr]((v1: hydra.ext.lisp.syntax.Expression) => hydra.ext.lisp.serde.expressionToExpr(d)(v1))(vl.elements)
  d match
    case hydra.ext.lisp.syntax.Dialect.clojure => hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(elems))
    case hydra.ext.lisp.syntax.Dialect.emacsLisp => hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(elems))
    case hydra.ext.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst("#"),
       hydra.serialization.parens(hydra.serialization.spaceSep(elems))))
    case hydra.ext.lisp.syntax.Dialect.scheme => hydra.serialization.noSep(Seq(hydra.serialization.cst("#"),
       hydra.serialization.parens(hydra.serialization.spaceSep(elems))))
}
