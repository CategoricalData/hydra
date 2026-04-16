package hydra.lisp.serde

import hydra.lisp.syntax.*

def andExpressionToExpr(d: hydra.lisp.syntax.Dialect)(andExpr: hydra.lisp.syntax.AndExpression): hydra.ast.Expr =
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("and")))(hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(andExpr.expressions))))

def applicationToExpr(d: hydra.lisp.syntax.Dialect)(app: hydra.lisp.syntax.Application): hydra.ast.Expr =
  {
  lazy val funExpr: hydra.lisp.syntax.Expression = (app.function)
  lazy val fun: hydra.ast.Expr = hydra.lisp.serde.expressionToExpr(d)(funExpr)
  lazy val args: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(app.arguments)
  lazy val needsFuncall: Boolean = d match
    case hydra.lisp.syntax.Dialect.emacsLisp => funExpr match
      case hydra.lisp.syntax.Expression.variable(v_Expression_variable_s) => false
      case _ => true
    case _ => false
  lazy val allParts: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](needsFuncall)(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("funcall"),
     fun))(args))(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(fun))(args))
  hydra.serialization.parens(hydra.serialization.spaceSep(allParts))
}

def caseExpressionToExpr(d: hydra.lisp.syntax.Dialect)(caseExpr: hydra.lisp.syntax.CaseExpression): hydra.ast.Expr =
  {
  lazy val scrutinee: hydra.ast.Expr = hydra.lisp.serde.expressionToExpr(d)(caseExpr.scrutinee)
  lazy val clauses: Seq[hydra.lisp.syntax.CaseClause] = (caseExpr.clauses)
  lazy val dflt: Option[hydra.lisp.syntax.Expression] = (caseExpr.default)
  lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.CaseClause,
     hydra.ast.Expr]((c: hydra.lisp.syntax.CaseClause) =>
    hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.lisp.syntax.Expression,
       hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(c.keys))),
       hydra.lisp.serde.expressionToExpr(d)(c.body)))))(clauses)
  lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr],
     hydra.lisp.syntax.Expression](Seq())((e: hydra.lisp.syntax.Expression) =>
    Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("else"),
       hydra.lisp.serde.expressionToExpr(d)(e))))))(dflt)
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("case"),
     scrutinee), clauseExprs, defaultPart))))
}

def commentToExpr(c: hydra.lisp.syntax.Comment): hydra.ast.Expr =
  {
  lazy val text: scala.Predef.String = (c.text)
  hydra.serialization.cst(hydra.lib.strings.cat2("; ")(text))
}

def condExpressionToExpr(d: hydra.lisp.syntax.Dialect)(condExpr: hydra.lisp.syntax.CondExpression): hydra.ast.Expr =
  {
  lazy val clauses: Seq[hydra.lisp.syntax.CondClause] = (condExpr.clauses)
  lazy val dflt: Option[hydra.lisp.syntax.Expression] = (condExpr.default)
  d match
    case hydra.lisp.syntax.Dialect.clojure => {
      lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[hydra.lisp.syntax.CondClause,
         Seq[hydra.ast.Expr]]((c: hydra.lisp.syntax.CondClause) =>
        Seq(hydra.lisp.serde.expressionToExpr(d)(c.condition), hydra.lisp.serde.expressionToExpr(d)(c.body)))(clauses))
      lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr],
         hydra.lisp.syntax.Expression](Seq())((e: hydra.lisp.syntax.Expression) =>
        Seq(hydra.serialization.cst(":else"), hydra.lisp.serde.expressionToExpr(d)(e)))(dflt)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cond")),
         clauseExprs, defaultPart))))
    }
    case hydra.lisp.syntax.Dialect.emacsLisp => {
      lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.CondClause,
         hydra.ast.Expr]((c: hydra.lisp.syntax.CondClause) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lisp.serde.expressionToExpr(d)(c.condition),
           hydra.lisp.serde.expressionToExpr(d)(c.body)))))(clauses)
      lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr],
         hydra.lisp.syntax.Expression](Seq())((e: hydra.lisp.syntax.Expression) =>
        Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("t"),
           hydra.lisp.serde.expressionToExpr(d)(e))))))(dflt)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cond")),
         clauseExprs, defaultPart))))
    }
    case hydra.lisp.syntax.Dialect.commonLisp => {
      lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.CondClause,
         hydra.ast.Expr]((c: hydra.lisp.syntax.CondClause) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lisp.serde.expressionToExpr(d)(c.condition),
           hydra.lisp.serde.expressionToExpr(d)(c.body)))))(clauses)
      lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr],
         hydra.lisp.syntax.Expression](Seq())((e: hydra.lisp.syntax.Expression) =>
        Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("t"),
           hydra.lisp.serde.expressionToExpr(d)(e))))))(dflt)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cond")),
         clauseExprs, defaultPart))))
    }
    case hydra.lisp.syntax.Dialect.scheme => {
      lazy val clauseExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.CondClause,
         hydra.ast.Expr]((c: hydra.lisp.syntax.CondClause) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lisp.serde.expressionToExpr(d)(c.condition),
           hydra.lisp.serde.expressionToExpr(d)(c.body)))))(clauses)
      lazy val defaultPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr],
         hydra.lisp.syntax.Expression](Seq())((e: hydra.lisp.syntax.Expression) =>
        Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("else"),
           hydra.lisp.serde.expressionToExpr(d)(e))))))(dflt)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cond")),
         clauseExprs, defaultPart))))
    }
}

def constantDefinitionToExpr(d: hydra.lisp.syntax.Dialect)(cdef: hydra.lisp.syntax.ConstantDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.lisp.serde.symbolToExpr(cdef.name)
  lazy val value: hydra.ast.Expr = hydra.lisp.serde.expressionToExpr(d)(cdef.value)
  hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(hydra.lisp.serde.defconstKeyword(d)),
     name, value)))
}

def defKeyword(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "def"
  case hydra.lisp.syntax.Dialect.emacsLisp => "defvar"
  case hydra.lisp.syntax.Dialect.commonLisp => "cl:defvar"
  case hydra.lisp.syntax.Dialect.scheme => "define"

def defconstKeyword(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "def"
  case hydra.lisp.syntax.Dialect.emacsLisp => "defconst"
  case hydra.lisp.syntax.Dialect.commonLisp => "cl:defconstant"
  case hydra.lisp.syntax.Dialect.scheme => "define"

def defnKeyword(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "defn"
  case hydra.lisp.syntax.Dialect.emacsLisp => "defun"
  case hydra.lisp.syntax.Dialect.commonLisp => "cl:defun"
  case hydra.lisp.syntax.Dialect.scheme => "define"

def defrecordKeyword(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "defrecord"
  case hydra.lisp.syntax.Dialect.emacsLisp => "cl-defstruct"
  case hydra.lisp.syntax.Dialect.commonLisp => "cl:defstruct"
  case hydra.lisp.syntax.Dialect.scheme => "define-record-type"

def doExpressionToExpr(d: hydra.lisp.syntax.Dialect)(doExpr: hydra.lisp.syntax.DoExpression): hydra.ast.Expr =
  {
  lazy val kw: scala.Predef.String = d match
    case hydra.lisp.syntax.Dialect.clojure => "do"
    case hydra.lisp.syntax.Dialect.emacsLisp => "progn"
    case hydra.lisp.syntax.Dialect.commonLisp => "progn"
    case hydra.lisp.syntax.Dialect.scheme => "begin"
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(kw)))(hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(doExpr.expressions))))
}

def docstringToExpr(ds: hydra.lisp.syntax.Docstring): hydra.ast.Expr = hydra.serialization.cst(hydra.lib.strings.cat(Seq(";; ",
   ds)))

def exportDeclarationToExpr(d: hydra.lisp.syntax.Dialect)(edecl: hydra.lisp.syntax.ExportDeclaration): hydra.ast.Expr =
  {
  lazy val syms: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Symbol,
     hydra.ast.Expr](hydra.lisp.serde.symbolToExpr)(edecl.symbols)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.cst("")
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ast.Expr,
       hydra.ast.Expr]((s: hydra.ast.Expr) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("provide"),
         hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), s))))))(syms))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(":export")))(hydra.lib.lists.map[hydra.ast.Expr,
       hydra.ast.Expr]((s: hydra.ast.Expr) =>
      hydra.serialization.noSep(Seq(hydra.serialization.cst(":"), s)))(syms))))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("export")))(syms)))
}

def expressionToExpr(d: hydra.lisp.syntax.Dialect)(expr: hydra.lisp.syntax.Expression): hydra.ast.Expr =
  expr match
  case hydra.lisp.syntax.Expression.application(v_Expression_application_a) => hydra.lisp.serde.applicationToExpr(d)(v_Expression_application_a)
  case hydra.lisp.syntax.Expression.lambda(v_Expression_lambda_l) => hydra.lisp.serde.lambdaToExpr(d)(v_Expression_lambda_l)
  case hydra.lisp.syntax.Expression.let(v_Expression_let_l) => hydra.lisp.serde.letExpressionToExpr(d)(v_Expression_let_l)
  case hydra.lisp.syntax.Expression.`if`(v_Expression_if_i) => hydra.lisp.serde.ifExpressionToExpr(d)(v_Expression_if_i)
  case hydra.lisp.syntax.Expression.cond(v_Expression_cond_c) => hydra.lisp.serde.condExpressionToExpr(d)(v_Expression_cond_c)
  case hydra.lisp.syntax.Expression.`case`(v_Expression_case_c) => hydra.lisp.serde.caseExpressionToExpr(d)(v_Expression_case_c)
  case hydra.lisp.syntax.Expression.and(v_Expression_and_a) => hydra.lisp.serde.andExpressionToExpr(d)(v_Expression_and_a)
  case hydra.lisp.syntax.Expression.or(v_Expression_or_o) => hydra.lisp.serde.orExpressionToExpr(d)(v_Expression_or_o)
  case hydra.lisp.syntax.Expression.not(v_Expression_not_n) => hydra.lisp.serde.notExpressionToExpr(d)(v_Expression_not_n)
  case hydra.lisp.syntax.Expression.`do`(v_Expression_do_e) => hydra.lisp.serde.doExpressionToExpr(d)(v_Expression_do_e)
  case hydra.lisp.syntax.Expression.begin(v_Expression_begin_e) => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("begin")))(hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(v_Expression_begin_e.expressions))))
  case hydra.lisp.syntax.Expression.variable(v_Expression_variable_v) => hydra.lisp.serde.variableReferenceToExpr(d)(v_Expression_variable_v)
  case hydra.lisp.syntax.Expression.literal(v_Expression_literal_l) => hydra.lisp.serde.literalToExpr(d)(v_Expression_literal_l)
  case hydra.lisp.syntax.Expression.list(v_Expression_list_l) => hydra.lisp.serde.listLiteralToExpr(d)(v_Expression_list_l)
  case hydra.lisp.syntax.Expression.vector(v_Expression_vector_v) => hydra.lisp.serde.vectorLiteralToExpr(d)(v_Expression_vector_v)
  case hydra.lisp.syntax.Expression.map(v_Expression_map_m) => hydra.lisp.serde.mapLiteralToExpr(d)(v_Expression_map_m)
  case hydra.lisp.syntax.Expression.set(v_Expression_set_s) => hydra.lisp.serde.setLiteralToExpr(d)(v_Expression_set_s)
  case hydra.lisp.syntax.Expression.cons(v_Expression_cons_c) => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("cons"),
     hydra.lisp.serde.expressionToExpr(d)(v_Expression_cons_c.head), hydra.lisp.serde.expressionToExpr(d)(v_Expression_cons_c.tail))))
  case hydra.lisp.syntax.Expression.dottedPair(v_Expression_dottedPair_p) => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lisp.serde.expressionToExpr(d)(v_Expression_dottedPair_p.car),
     hydra.serialization.cst("."), hydra.lisp.serde.expressionToExpr(d)(v_Expression_dottedPair_p.cdr))))
  case hydra.lisp.syntax.Expression.fieldAccess(v_Expression_fieldAccess_fa) => hydra.lisp.serde.fieldAccessToExpr(d)(v_Expression_fieldAccess_fa)
  case hydra.lisp.syntax.Expression.typeAnnotation(v_Expression_typeAnnotation_ta) => hydra.lisp.serde.expressionToExpr(d)(v_Expression_typeAnnotation_ta.expression)
  case hydra.lisp.syntax.Expression.quote(v_Expression_quote_q) => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
     hydra.lisp.serde.expressionToExpr(d)(v_Expression_quote_q.body)))
  case hydra.lisp.syntax.Expression.quasiquote(v_Expression_quasiquote_q) => hydra.serialization.noSep(Seq(hydra.serialization.cst("`"),
     hydra.lisp.serde.expressionToExpr(d)(v_Expression_quasiquote_q.body)))
  case hydra.lisp.syntax.Expression.unquote(v_Expression_unquote_u) => d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.noSep(Seq(hydra.serialization.cst("~"),
       hydra.lisp.serde.expressionToExpr(d)(v_Expression_unquote_u.body)))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst(","),
       hydra.lisp.serde.expressionToExpr(d)(v_Expression_unquote_u.body)))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst(","),
       hydra.lisp.serde.expressionToExpr(d)(v_Expression_unquote_u.body)))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.noSep(Seq(hydra.serialization.cst(","),
       hydra.lisp.serde.expressionToExpr(d)(v_Expression_unquote_u.body)))
  case hydra.lisp.syntax.Expression.splicingUnquote(v_Expression_splicingUnquote_su) => d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.noSep(Seq(hydra.serialization.cst("~@"),
       hydra.lisp.serde.expressionToExpr(d)(v_Expression_splicingUnquote_su.body)))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst(",@"),
       hydra.lisp.serde.expressionToExpr(d)(v_Expression_splicingUnquote_su.body)))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst(",@"),
       hydra.lisp.serde.expressionToExpr(d)(v_Expression_splicingUnquote_su.body)))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.noSep(Seq(hydra.serialization.cst(",@"),
       hydra.lisp.serde.expressionToExpr(d)(v_Expression_splicingUnquote_su.body)))
  case hydra.lisp.syntax.Expression.sExpression(v_Expression_sExpression_s) => hydra.lisp.serde.sExpressionToExpr(v_Expression_sExpression_s)

def falseExpr(d: hydra.lisp.syntax.Dialect): hydra.ast.Expr =
  d match
  case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.cst("false")
  case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.cst("nil")
  case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.cst("cl:nil")
  case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.cst("#f")

def fieldAccessToExpr(d: hydra.lisp.syntax.Dialect)(fa: hydra.lisp.syntax.FieldAccess): hydra.ast.Expr =
  {
  lazy val rtype: hydra.ast.Expr = hydra.lisp.serde.symbolToExpr(fa.recordType)
  lazy val field: hydra.ast.Expr = hydra.lisp.serde.symbolToExpr(fa.field)
  lazy val target: hydra.ast.Expr = hydra.lisp.serde.expressionToExpr(d)(fa.target)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(hydra.serialization.cst(":"),
       field)), target)))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(rtype,
       hydra.serialization.cst("-"), field)), target)))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(rtype,
       hydra.serialization.cst("-"), field)), target)))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(rtype,
       hydra.serialization.cst("-"), field)), target)))
}

def formatLispFloat(d: hydra.lisp.syntax.Dialect)(v: BigDecimal): scala.Predef.String =
  {
  lazy val s: scala.Predef.String = hydra.lib.literals.showBigfloat(v)
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](s)("NaN"))(d match
    case hydra.lisp.syntax.Dialect.clojure => "Double/NaN"
    case hydra.lisp.syntax.Dialect.scheme => "+nan.0"
    case hydra.lisp.syntax.Dialect.commonLisp => "+hydra-nan+"
    case hydra.lisp.syntax.Dialect.emacsLisp => "0.0e+NaN")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](s)("Infinity"))(d match
    case hydra.lisp.syntax.Dialect.clojure => "Double/POSITIVE_INFINITY"
    case hydra.lisp.syntax.Dialect.scheme => "+inf.0"
    case hydra.lisp.syntax.Dialect.commonLisp => "+hydra-pos-inf+"
    case hydra.lisp.syntax.Dialect.emacsLisp => "1.0e+INF")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](s)("-Infinity"))(d match
    case hydra.lisp.syntax.Dialect.clojure => "Double/NEGATIVE_INFINITY"
    case hydra.lisp.syntax.Dialect.scheme => "-inf.0"
    case hydra.lisp.syntax.Dialect.commonLisp => "+hydra-neg-inf+"
    case hydra.lisp.syntax.Dialect.emacsLisp => "-1.0e+INF")(s)))
}

def functionDefinitionToExpr(d: hydra.lisp.syntax.Dialect)(fdef: hydra.lisp.syntax.FunctionDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.lisp.serde.symbolToExpr(fdef.name)
  lazy val params: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Symbol,
     hydra.ast.Expr](hydra.lisp.serde.symbolToExpr)(fdef.params)
  lazy val body: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(fdef.body)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defn"),
       name), Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(params))),
       body))))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defun"),
       name), Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))),
       body))))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defun"),
       name), Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))),
       body))))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("define")),
       Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(name))(params)))),
       body))))
}

def ifExpressionToExpr(d: hydra.lisp.syntax.Dialect)(ifExpr: hydra.lisp.syntax.IfExpression): hydra.ast.Expr =
  {
  lazy val cond: hydra.ast.Expr = hydra.lisp.serde.expressionToExpr(d)(ifExpr.condition)
  lazy val `then`: hydra.ast.Expr = hydra.lisp.serde.expressionToExpr(d)(ifExpr.`then`)
  lazy val `else`: Option[hydra.lisp.syntax.Expression] = (ifExpr.`else`)
  lazy val elsePart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr],
     hydra.lisp.syntax.Expression](Seq())((e: hydra.lisp.syntax.Expression) => Seq(hydra.lisp.serde.expressionToExpr(d)(e)))(`else`)
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("if"),
     cond, `then`), elsePart))))
}

def importDeclarationToExpr(d: hydra.lisp.syntax.Dialect)(idecl: hydra.lisp.syntax.ImportDeclaration): hydra.ast.Expr =
  {
  lazy val modName: scala.Predef.String = (idecl.module)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(":require"),
       hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(modName)))))))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("require"),
       hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst(modName))))))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(":use"),
       hydra.serialization.cst(hydra.lib.strings.cat2(":")(modName)))))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("import"),
       hydra.serialization.parens(hydra.serialization.cst(modName)))))
}

def keywordToExpr(d: hydra.lisp.syntax.Dialect)(k: hydra.lisp.syntax.Keyword): hydra.ast.Expr =
  {
  lazy val name: scala.Predef.String = (k.name)
  lazy val ns: Option[scala.Predef.String] = (k.namespace)
  d match
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
       hydra.serialization.cst(name)))
    case _ => hydra.serialization.cst(hydra.lib.maybes.maybe[scala.Predef.String,
       scala.Predef.String](hydra.lib.strings.cat2(":")(name))((n: scala.Predef.String) => hydra.lib.strings.cat(Seq(n,
       "/:", name)))(ns))
}

def lambdaKeyword(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "fn"
  case hydra.lisp.syntax.Dialect.emacsLisp => "lambda"
  case hydra.lisp.syntax.Dialect.commonLisp => "cl:lambda"
  case hydra.lisp.syntax.Dialect.scheme => "lambda"

def lambdaToExpr(d: hydra.lisp.syntax.Dialect)(lam: hydra.lisp.syntax.Lambda): hydra.ast.Expr =
  {
  lazy val params: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Symbol,
     hydra.ast.Expr](hydra.lisp.serde.symbolToExpr)(lam.params)
  lazy val body: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(lam.body)
  lazy val mname: Option[hydra.lisp.syntax.Symbol] = (lam.name)
  lazy val kw: scala.Predef.String = hydra.lisp.serde.lambdaKeyword(d)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.lib.maybes.maybe[hydra.ast.Expr,
       hydra.lisp.syntax.Symbol](hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
       Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(params))),
       body)))))((sym: hydra.lisp.syntax.Symbol) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw),
         hydra.lisp.serde.symbolToExpr(sym)), Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(params))),
         body)))))(mname)
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
       Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
       Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
       Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))), body))))
}

def letExpressionToExpr(d: hydra.lisp.syntax.Dialect)(letExpr: hydra.lisp.syntax.LetExpression): hydra.ast.Expr =
  {
  lazy val kind: hydra.lisp.syntax.LetKind = (letExpr.kind)
  lazy val bindings: Seq[hydra.lisp.syntax.LetBinding] = (letExpr.bindings)
  lazy val body: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(letExpr.body)
  lazy val bindingPairs: Seq[Tuple2[hydra.ast.Expr, hydra.ast.Expr]] = hydra.lib.lists.map[hydra.lisp.syntax.LetBinding,
     Tuple2[hydra.ast.Expr, hydra.ast.Expr]]((b: hydra.lisp.syntax.LetBinding) =>
    b match
    case hydra.lisp.syntax.LetBinding.simple(v_LetBinding_simple_sb) => Tuple2(hydra.lisp.serde.symbolToExpr(v_LetBinding_simple_sb.name),
       hydra.lisp.serde.expressionToExpr(d)(v_LetBinding_simple_sb.value))
    case hydra.lisp.syntax.LetBinding.destructuring(v_LetBinding_destructuring__) => Tuple2(hydra.serialization.cst("<destructuring>"),
       hydra.serialization.cst("<destructuring>")))(bindings)
  d match
    case hydra.lisp.syntax.Dialect.clojure => kind match
      case hydra.lisp.syntax.LetKind.recursive => {
        lazy val letfnBindings: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.LetBinding,
           hydra.ast.Expr]((b: hydra.lisp.syntax.LetBinding) =>
          b match
          case hydra.lisp.syntax.LetBinding.simple(v_LetBinding_simple_sb) => {
            lazy val sbName: hydra.ast.Expr = hydra.lisp.serde.symbolToExpr(v_LetBinding_simple_sb.name)
            {
              lazy val sbVal: hydra.lisp.syntax.Expression = (v_LetBinding_simple_sb.value)
              sbVal match
                case hydra.lisp.syntax.Expression.lambda(v_Expression_lambda_lam) => {
                  lazy val params: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Symbol,
                     hydra.ast.Expr](hydra.lisp.serde.symbolToExpr)(v_Expression_lambda_lam.params)
                  {
                    lazy val lamBody: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Expression,
                       hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(v_Expression_lambda_lam.body)
                    hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(sbName),
                       Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(params))),
                       lamBody))))
                  }
                }
                case _ => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(sbName,
                   hydra.lisp.serde.expressionToExpr(d)(sbVal))))
            }
          }
          case hydra.lisp.syntax.LetBinding.destructuring(v_LetBinding_destructuring__3) => hydra.serialization.cst("<destructuring>"))(bindings)
        hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("letfn")),
           Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(letfnBindings))),
           body))))
      }
      case hydra.lisp.syntax.LetKind.parallel => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("let")),
         Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[Tuple2[hydra.ast.Expr,
         hydra.ast.Expr], Seq[hydra.ast.Expr]]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        Seq(hydra.lib.pairs.first[hydra.ast.Expr, hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr,
           hydra.ast.Expr](p)))(bindingPairs))))), body))))
      case hydra.lisp.syntax.LetKind.sequential => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("let")),
         Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[Tuple2[hydra.ast.Expr,
         hydra.ast.Expr], Seq[hydra.ast.Expr]]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        Seq(hydra.lib.pairs.first[hydra.ast.Expr, hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr,
           hydra.ast.Expr](p)))(bindingPairs))))), body))))
    case hydra.lisp.syntax.Dialect.emacsLisp => {
      lazy val kw: scala.Predef.String = kind match
        case hydra.lisp.syntax.LetKind.parallel => "let"
        case hydra.lisp.syntax.LetKind.sequential => "let*"
        case hydra.lisp.syntax.LetKind.recursive => "letrec"
      lazy val bindingExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[Tuple2[hydra.ast.Expr,
         hydra.ast.Expr], hydra.ast.Expr]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lib.pairs.first[hydra.ast.Expr,
           hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr, hydra.ast.Expr](p)))))(bindingPairs)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
         Seq(hydra.serialization.parens(hydra.serialization.spaceSep(bindingExprs))),
         body))))
    }
    case hydra.lisp.syntax.Dialect.commonLisp => {
      lazy val kw: scala.Predef.String = kind match
        case hydra.lisp.syntax.LetKind.parallel => "let"
        case hydra.lisp.syntax.LetKind.sequential => "let*"
        case hydra.lisp.syntax.LetKind.recursive => "letrec"
      lazy val bindingExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[Tuple2[hydra.ast.Expr,
         hydra.ast.Expr], hydra.ast.Expr]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lib.pairs.first[hydra.ast.Expr,
           hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr, hydra.ast.Expr](p)))))(bindingPairs)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
         Seq(hydra.serialization.parens(hydra.serialization.spaceSep(bindingExprs))),
         body))))
    }
    case hydra.lisp.syntax.Dialect.scheme => {
      lazy val kw: scala.Predef.String = kind match
        case hydra.lisp.syntax.LetKind.parallel => "let"
        case hydra.lisp.syntax.LetKind.sequential => "let*"
        case hydra.lisp.syntax.LetKind.recursive => "letrec"
      lazy val bindingExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[Tuple2[hydra.ast.Expr,
         hydra.ast.Expr], hydra.ast.Expr]((p: Tuple2[hydra.ast.Expr, hydra.ast.Expr]) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lib.pairs.first[hydra.ast.Expr,
           hydra.ast.Expr](p), hydra.lib.pairs.second[hydra.ast.Expr, hydra.ast.Expr](p)))))(bindingPairs)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst(kw)),
         Seq(hydra.serialization.parens(hydra.serialization.spaceSep(bindingExprs))),
         body))))
    }
}

def listKeyword(d: hydra.lisp.syntax.Dialect): scala.Predef.String =
  d match
  case hydra.lisp.syntax.Dialect.clojure => "list"
  case hydra.lisp.syntax.Dialect.emacsLisp => "list"
  case hydra.lisp.syntax.Dialect.commonLisp => "cl:list"
  case hydra.lisp.syntax.Dialect.scheme => "list"

def listLiteralToExpr(d: hydra.lisp.syntax.Dialect)(ll: hydra.lisp.syntax.ListLiteral): hydra.ast.Expr =
  {
  lazy val elems: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(ll.elements)
  lazy val quoted: Boolean = (ll.quoted)
  hydra.lib.logic.ifElse[hydra.ast.Expr](quoted)(hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
     hydra.serialization.parens(hydra.serialization.spaceSep(elems)))))(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(hydra.lisp.serde.listKeyword(d))))(elems))))
}

def literalToExpr(d: hydra.lisp.syntax.Dialect)(lit: hydra.lisp.syntax.Literal): hydra.ast.Expr =
  lit match
  case hydra.lisp.syntax.Literal.integer(v_Literal_integer_i) => hydra.serialization.cst(hydra.lib.literals.showBigint(v_Literal_integer_i.value))
  case hydra.lisp.syntax.Literal.float(v_Literal_float_f) => hydra.serialization.cst(hydra.lisp.serde.formatLispFloat(d)(v_Literal_float_f.value))
  case hydra.lisp.syntax.Literal.string(v_Literal_string_s) => {
    lazy val e1: scala.Predef.String = hydra.lib.strings.intercalate("\\\\")(hydra.lib.strings.splitOn("\\")(v_Literal_string_s))
    d match
      case hydra.lisp.syntax.Dialect.commonLisp => {
        lazy val escaped: scala.Predef.String = hydra.lib.strings.intercalate("\\\"")(hydra.lib.strings.splitOn("\"")(e1))
        hydra.serialization.cst(hydra.lib.strings.cat(Seq("\"", escaped, "\"")))
      }
      case hydra.lisp.syntax.Dialect.clojure => {
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
      case hydra.lisp.syntax.Dialect.emacsLisp => {
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
      case hydra.lisp.syntax.Dialect.scheme => {
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
  case hydra.lisp.syntax.Literal.character(v_Literal_character_c) => {
    lazy val ch: scala.Predef.String = (v_Literal_character_c.value)
    d match
      case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.cst(hydra.lib.strings.cat2("\\")(ch))
      case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.cst(hydra.lib.strings.cat2("?")(ch))
      case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.cst(hydra.lib.strings.cat2("#\\")(ch))
      case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.cst(hydra.lib.strings.cat2("#\\")(ch))
  }
  case hydra.lisp.syntax.Literal.boolean(v_Literal_boolean_b) => hydra.lib.logic.ifElse[hydra.ast.Expr](v_Literal_boolean_b)(hydra.lisp.serde.trueExpr(d))(hydra.lisp.serde.falseExpr(d))
  case hydra.lisp.syntax.Literal.nil => hydra.lisp.serde.nilExpr(d)
  case hydra.lisp.syntax.Literal.keyword(v_Literal_keyword_k) => hydra.lisp.serde.keywordToExpr(d)(v_Literal_keyword_k)
  case hydra.lisp.syntax.Literal.symbol(v_Literal_symbol_s) => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
     hydra.lisp.serde.symbolToExpr(v_Literal_symbol_s)))

def macroDefinitionToExpr(d: hydra.lisp.syntax.Dialect)(mdef: hydra.lisp.syntax.MacroDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.lisp.serde.symbolToExpr(mdef.name)
  lazy val params: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Symbol,
     hydra.ast.Expr](hydra.lisp.serde.symbolToExpr)(mdef.params)
  lazy val body: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(mdef.body)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defmacro"),
       name), Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(params))),
       body))))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defmacro"),
       name), Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))),
       body))))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defmacro"),
       name), Seq(hydra.serialization.parens(hydra.serialization.spaceSep(params))),
       body))))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("define-syntax"),
       name), body))))
}

def mapLiteralToExpr(d: hydra.lisp.syntax.Dialect)(ml: hydra.lisp.syntax.MapLiteral): hydra.ast.Expr =
  {
  lazy val entries: Seq[hydra.lisp.syntax.MapEntry] = (ml.entries)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.brackets(hydra.serialization.curlyBraces)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[hydra.lisp.syntax.MapEntry,
       Seq[hydra.ast.Expr]]((e: hydra.lisp.syntax.MapEntry) =>
      Seq(hydra.lisp.serde.expressionToExpr(d)(e.key), hydra.lisp.serde.expressionToExpr(d)(e.value)))(entries))))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
       hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.lisp.syntax.MapEntry,
       hydra.ast.Expr]((e: hydra.lisp.syntax.MapEntry) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lisp.serde.expressionToExpr(d)(e.key),
         hydra.serialization.cst("."), hydra.lisp.serde.expressionToExpr(d)(e.value)))))(entries)))))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst("'"),
       hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.lisp.syntax.MapEntry,
       hydra.ast.Expr]((e: hydra.lisp.syntax.MapEntry) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.lisp.serde.expressionToExpr(d)(e.key),
         hydra.serialization.cst("."), hydra.lisp.serde.expressionToExpr(d)(e.value)))))(entries)))))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("list")))(hydra.lib.lists.map[hydra.lisp.syntax.MapEntry,
       hydra.ast.Expr]((e: hydra.lisp.syntax.MapEntry) =>
      hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("cons"),
         hydra.lisp.serde.expressionToExpr(d)(e.key), hydra.lisp.serde.expressionToExpr(d)(e.value)))))(entries))))
}

def moduleDeclarationToExpr(d: hydra.lisp.syntax.Dialect)(mdecl: hydra.lisp.syntax.ModuleDeclaration): hydra.ast.Expr =
  {
  lazy val name: scala.Predef.String = (mdecl.name)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("ns"),
       hydra.serialization.cst(name))))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.newlineSep(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("require"),
       hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst("cl-lib")))))),
       hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("provide"),
       hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst(name))))))))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.newlineSep(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("defpackage"),
       hydra.serialization.cst(hydra.lib.strings.cat2(":")(name))))), hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("in-package"),
       hydra.serialization.cst(hydra.lib.strings.cat2(":")(name)))))))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("define-library"),
       hydra.serialization.parens(hydra.serialization.cst(name)))))
}

def nilExpr(d: hydra.lisp.syntax.Dialect): hydra.ast.Expr =
  d match
  case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.cst("nil")
  case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.cst("nil")
  case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.cst("cl:nil")
  case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.cst("'()")

def notExpressionToExpr(d: hydra.lisp.syntax.Dialect)(notExpr: hydra.lisp.syntax.NotExpression): hydra.ast.Expr =
  hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("not"),
     hydra.lisp.serde.expressionToExpr(d)(notExpr.expression))))

def orExpressionToExpr(d: hydra.lisp.syntax.Dialect)(orExpr: hydra.lisp.syntax.OrExpression): hydra.ast.Expr =
  hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("or")))(hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(orExpr.expressions))))

def programToExpr(prog: hydra.lisp.syntax.Program): hydra.ast.Expr =
  {
  lazy val d: hydra.lisp.syntax.Dialect = (prog.dialect)
  lazy val modDecl: Option[hydra.lisp.syntax.ModuleDeclaration] = (prog.module)
  lazy val imports: Seq[hydra.lisp.syntax.ImportDeclaration] = (prog.imports)
  lazy val exports: Seq[hydra.lisp.syntax.ExportDeclaration] = (prog.exports)
  lazy val forms: Seq[hydra.lisp.syntax.TopLevelFormWithComments] = (prog.forms)
  lazy val formPart: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.TopLevelFormWithComments,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.TopLevelFormWithComments) => hydra.lisp.serde.topLevelFormWithCommentsToExpr(d)(v1))(forms)
  lazy val importNames: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.lisp.syntax.ImportDeclaration,
     scala.Predef.String]((idecl: hydra.lisp.syntax.ImportDeclaration) => (idecl.module))(imports)
  lazy val exportSyms: Seq[hydra.ast.Expr] = hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[hydra.lisp.syntax.ExportDeclaration,
     Seq[hydra.ast.Expr]]((edecl: hydra.lisp.syntax.ExportDeclaration) =>
    hydra.lib.lists.map[hydra.lisp.syntax.Symbol, hydra.ast.Expr](hydra.lisp.serde.symbolToExpr)(edecl.symbols))(exports))
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.lib.maybes.maybe[hydra.ast.Expr,
       hydra.lisp.syntax.ModuleDeclaration](hydra.serialization.doubleNewlineSep(formPart))((m: hydra.lisp.syntax.ModuleDeclaration) =>
      {
      lazy val nameStr: scala.Predef.String = (m.name)
      lazy val requireClauses: Seq[hydra.ast.Expr] = hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((imp: scala.Predef.String) =>
        hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(imp),
           hydra.serialization.cst(":refer"), hydra.serialization.cst(":all")))))(importNames)
      lazy val nsForm: hydra.ast.Expr = hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ast.Expr](requireClauses))(hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("ns"),
         hydra.serialization.cst(nameStr)))))(hydra.serialization.parens(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("ns"),
         hydra.serialization.cst(nameStr))), hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("  (:require")))(requireClauses)),
         hydra.serialization.cst(")")))))
      {
        lazy val varNames: Seq[hydra.ast.Expr] = hydra.lib.lists.concat[hydra.ast.Expr](hydra.lib.lists.map[hydra.lisp.syntax.TopLevelFormWithComments,
           Seq[hydra.ast.Expr]]((fwc: hydra.lisp.syntax.TopLevelFormWithComments) =>
          {
          lazy val form: hydra.lisp.syntax.TopLevelForm = (fwc.form)
          form match
            case hydra.lisp.syntax.TopLevelForm.variable(v_TopLevelForm_variable_vd) => Seq(hydra.lisp.serde.symbolToExpr(v_TopLevelForm_variable_vd.name))
            case hydra.lisp.syntax.TopLevelForm.function(v_TopLevelForm_function_fd) => Seq(hydra.lisp.serde.symbolToExpr(v_TopLevelForm_function_fd.name))
            case _ => Seq()
        })(forms))
        {
          lazy val declareForm: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ast.Expr](varNames))(Seq())(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("declare")))(varNames)))))
          hydra.serialization.doubleNewlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(nsForm),
             declareForm, formPart)))
        }
      }
    })(modDecl)
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.lib.maybes.maybe[hydra.ast.Expr,
       hydra.lisp.syntax.ModuleDeclaration](hydra.serialization.doubleNewlineSep(formPart))((m: hydra.lisp.syntax.ModuleDeclaration) =>
      {
      lazy val nameStr: scala.Predef.String = (m.name)
      lazy val requireClLib: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("require"),
         hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst("cl-lib"))))))
      lazy val requireImports: Seq[hydra.ast.Expr] = hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((imp: scala.Predef.String) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("require"),
           hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst(imp)))))))(importNames)
      lazy val provideForm: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("provide"),
         hydra.serialization.noSep(Seq(hydra.serialization.cst("'"), hydra.serialization.cst(nameStr))))))
      hydra.serialization.doubleNewlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(requireClLib),
         requireImports, formPart, Seq(provideForm))))
    })(modDecl)
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.lib.maybes.maybe[hydra.ast.Expr,
       hydra.lisp.syntax.ModuleDeclaration](hydra.serialization.doubleNewlineSep(formPart))((m: hydra.lisp.syntax.ModuleDeclaration) =>
      {
      lazy val nameStr: scala.Predef.String = (m.name)
      lazy val colonName: scala.Predef.String = hydra.lib.strings.cat2(":")(nameStr)
      lazy val useClause: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(":use"),
         hydra.serialization.cst(":cl")))(hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((imp: scala.Predef.String) => hydra.serialization.cst(hydra.lib.strings.cat2(":")(imp)))(importNames))))
      lazy val exportClause: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ast.Expr](exportSyms))(Seq())(Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(":export")))(hydra.lib.lists.map[hydra.ast.Expr,
         hydra.ast.Expr]((s: hydra.ast.Expr) =>
        hydra.serialization.noSep(Seq(hydra.serialization.cst(":"), s)))(exportSyms))))))
      lazy val defpkgForm: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.newlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("defpackage"),
         hydra.serialization.cst(colonName)))), Seq(useClause), exportClause))))
      lazy val inpkgForm: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("in-package"),
         hydra.serialization.cst(colonName))))
      hydra.serialization.doubleNewlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(defpkgForm,
         inpkgForm), formPart)))
    })(modDecl)
    case hydra.lisp.syntax.Dialect.scheme => hydra.lib.maybes.maybe[hydra.ast.Expr,
       hydra.lisp.syntax.ModuleDeclaration](hydra.serialization.doubleNewlineSep(formPart))((m: hydra.lisp.syntax.ModuleDeclaration) =>
      {
      lazy val nameStr: scala.Predef.String = (m.name)
      lazy val nameParts: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String,
         scala.Predef.String]((p: scala.Predef.String) => hydra.formatting.convertCaseCamelToLowerSnake(p))(hydra.lib.strings.splitOn(".")(nameStr))
      lazy val nameExpr: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((p: scala.Predef.String) => hydra.serialization.cst(p))(nameParts)))
      lazy val domainImportExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.ImportDeclaration,
         hydra.ast.Expr]((idecl: hydra.lisp.syntax.ImportDeclaration) =>
        {
        lazy val nsName: scala.Predef.String = (idecl.module)
        lazy val nsParts: Seq[scala.Predef.String] = hydra.lib.lists.map[scala.Predef.String,
           scala.Predef.String]((p: scala.Predef.String) => hydra.formatting.convertCaseCamelToLowerSnake(p))(hydra.lib.strings.splitOn(".")(nsName))
        hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[scala.Predef.String,
           hydra.ast.Expr]((p: scala.Predef.String) => hydra.serialization.cst(p))(nsParts)))
      })(imports)
      lazy val schemeBaseExpr: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("scheme"),
         hydra.serialization.cst("base"))))
      lazy val allImportExprs: Seq[hydra.ast.Expr] = hydra.lib.lists.concat2[hydra.ast.Expr](Seq(schemeBaseExpr))(domainImportExprs)
      lazy val importClause: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("import")))(allImportExprs)))
      lazy val exportClauses: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.ExportDeclaration,
         hydra.ast.Expr]((edecl: hydra.lisp.syntax.ExportDeclaration) => hydra.lisp.serde.exportDeclarationToExpr(d)(edecl))(exports)
      lazy val beginClause: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.newlineSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("begin")))(formPart)))
      hydra.serialization.parens(hydra.serialization.newlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("define-library"),
         nameExpr))), exportClauses, Seq(importClause), Seq(beginClause)))))
    })(modDecl)
}

def recordTypeDefinitionToExpr(d: hydra.lisp.syntax.Dialect)(rdef: hydra.lisp.syntax.RecordTypeDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.lisp.serde.symbolToExpr(rdef.name)
  lazy val fields: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.FieldDefinition,
     hydra.ast.Expr]((f: hydra.lisp.syntax.FieldDefinition) => hydra.lisp.serde.symbolToExpr(f.name))(rdef.fields)
  d match
    case hydra.lisp.syntax.Dialect.clojure => {
      lazy val nameStr: scala.Predef.String = (rdef.name)
      lazy val fieldNames: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.lisp.syntax.FieldDefinition,
         scala.Predef.String]((f: hydra.lisp.syntax.FieldDefinition) => (f.name))(rdef.fields)
      lazy val defrecordForm: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("defrecord"),
         name, hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(fields)))))
      lazy val makeAlias: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("defn"),
         hydra.serialization.cst(hydra.lib.strings.cat2("make-")(nameStr))), Seq(hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(fields))),
         Seq(hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(hydra.lib.strings.cat2("->")(nameStr))))(hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((fn: scala.Predef.String) => hydra.serialization.cst(fn))(fieldNames)))))))))
      hydra.serialization.newlineSep(Seq(defrecordForm, makeAlias))
    }
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cl-defstruct"),
       name), fields))))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("cl:defstruct"),
       name), fields))))
    case hydra.lisp.syntax.Dialect.scheme => {
      lazy val nameStr: scala.Predef.String = (rdef.name)
      lazy val fieldNames: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.lisp.syntax.FieldDefinition,
         scala.Predef.String]((f: hydra.lisp.syntax.FieldDefinition) => (f.name))(rdef.fields)
      lazy val constructor: hydra.ast.Expr = hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst(hydra.lib.strings.cat2("make-")(nameStr))))(hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((fn: scala.Predef.String) => hydra.serialization.cst(fn))(fieldNames))))
      lazy val predicate: hydra.ast.Expr = hydra.serialization.cst(hydra.lib.strings.cat2(nameStr)("?"))
      lazy val accessors: Seq[hydra.ast.Expr] = hydra.lib.lists.map[scala.Predef.String,
         hydra.ast.Expr]((fn: scala.Predef.String) =>
        hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(fn),
           hydra.serialization.cst(hydra.lib.strings.cat(Seq(nameStr, "-", fn)))))))(fieldNames)
      hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(Seq(hydra.serialization.cst("define-record-type"),
         name, constructor, predicate), accessors))))
    }
}

def sExpressionToExpr(sexpr: hydra.lisp.syntax.SExpression): hydra.ast.Expr =
  sexpr match
  case hydra.lisp.syntax.SExpression.atom(v_SExpression_atom_a) => hydra.serialization.cst(v_SExpression_atom_a)
  case hydra.lisp.syntax.SExpression.list(v_SExpression_list_elems) => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.lisp.syntax.SExpression,
     hydra.ast.Expr](hydra.lisp.serde.sExpressionToExpr)(v_SExpression_list_elems)))

def setLiteralToExpr(d: hydra.lisp.syntax.Dialect)(sl: hydra.lisp.syntax.SetLiteral): hydra.ast.Expr =
  {
  lazy val elems: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(sl.elements)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.noSep(Seq(hydra.serialization.cst("#"),
       hydra.serialization.brackets(hydra.serialization.curlyBraces)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(elems))))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("list")))(elems)))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("cl:list")))(elems)))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.parens(hydra.serialization.spaceSep(hydra.lib.lists.concat2[hydra.ast.Expr](Seq(hydra.serialization.cst("list")))(elems)))
}

def symbolToExpr(s: hydra.lisp.syntax.Symbol): hydra.ast.Expr = hydra.serialization.cst(s)

def topLevelFormToExpr(d: hydra.lisp.syntax.Dialect)(form: hydra.lisp.syntax.TopLevelForm): hydra.ast.Expr =
  form match
  case hydra.lisp.syntax.TopLevelForm.function(v_TopLevelForm_function_f) => hydra.lisp.serde.functionDefinitionToExpr(d)(v_TopLevelForm_function_f)
  case hydra.lisp.syntax.TopLevelForm.variable(v_TopLevelForm_variable_v) => hydra.lisp.serde.variableDefinitionToExpr(d)(v_TopLevelForm_variable_v)
  case hydra.lisp.syntax.TopLevelForm.constant(v_TopLevelForm_constant_c) => hydra.lisp.serde.constantDefinitionToExpr(d)(v_TopLevelForm_constant_c)
  case hydra.lisp.syntax.TopLevelForm.recordType(v_TopLevelForm_recordType_r) => hydra.lisp.serde.recordTypeDefinitionToExpr(d)(v_TopLevelForm_recordType_r)
  case hydra.lisp.syntax.TopLevelForm.`macro`(v_TopLevelForm_macro_m) => hydra.lisp.serde.macroDefinitionToExpr(d)(v_TopLevelForm_macro_m)
  case hydra.lisp.syntax.TopLevelForm.expression(v_TopLevelForm_expression_e) => hydra.lisp.serde.expressionToExpr(d)(v_TopLevelForm_expression_e)

def topLevelFormWithCommentsToExpr(d: hydra.lisp.syntax.Dialect)(fwc: hydra.lisp.syntax.TopLevelFormWithComments): hydra.ast.Expr =
  {
  lazy val mdoc: Option[hydra.lisp.syntax.Docstring] = (fwc.doc)
  lazy val mcomment: Option[hydra.lisp.syntax.Comment] = (fwc.comment)
  lazy val form: hydra.lisp.syntax.TopLevelForm = (fwc.form)
  lazy val docPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr],
     hydra.lisp.syntax.Docstring](Seq())((ds: hydra.lisp.syntax.Docstring) => Seq(hydra.lisp.serde.docstringToExpr(ds)))(mdoc)
  lazy val commentPart: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr],
     hydra.lisp.syntax.Comment](Seq())((c: hydra.lisp.syntax.Comment) => Seq(hydra.lisp.serde.commentToExpr(c)))(mcomment)
  lazy val formExpr: hydra.ast.Expr = hydra.lisp.serde.topLevelFormToExpr(d)(form)
  hydra.serialization.newlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(commentPart, docPart, Seq(formExpr))))
}

def trueExpr(d: hydra.lisp.syntax.Dialect): hydra.ast.Expr =
  d match
  case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.cst("true")
  case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.cst("t")
  case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.cst("cl:t")
  case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.cst("#t")

def variableDefinitionToExpr(d: hydra.lisp.syntax.Dialect)(vdef: hydra.lisp.syntax.VariableDefinition): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.lisp.serde.symbolToExpr(vdef.name)
  lazy val value: hydra.ast.Expr = hydra.lisp.serde.expressionToExpr(d)(vdef.value)
  hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst(hydra.lisp.serde.defKeyword(d)),
     name, value)))
}

def variableReferenceToExpr(d: hydra.lisp.syntax.Dialect)(vref: hydra.lisp.syntax.VariableReference): hydra.ast.Expr =
  {
  lazy val name: hydra.ast.Expr = hydra.lisp.serde.symbolToExpr(vref.name)
  lazy val isFnNs: Boolean = (vref.functionNamespace)
  hydra.lib.logic.ifElse[hydra.ast.Expr](isFnNs)(d match
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst("#'"), name))
    case hydra.lisp.syntax.Dialect.clojure => name
    case hydra.lisp.syntax.Dialect.emacsLisp => name
    case hydra.lisp.syntax.Dialect.scheme => name)(name)
}

def vectorLiteralToExpr(d: hydra.lisp.syntax.Dialect)(vl: hydra.lisp.syntax.VectorLiteral): hydra.ast.Expr =
  {
  lazy val elems: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.lisp.syntax.Expression,
     hydra.ast.Expr]((v1: hydra.lisp.syntax.Expression) => hydra.lisp.serde.expressionToExpr(d)(v1))(vl.elements)
  d match
    case hydra.lisp.syntax.Dialect.clojure => hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(elems))
    case hydra.lisp.syntax.Dialect.emacsLisp => hydra.serialization.brackets(hydra.serialization.squareBrackets)(hydra.serialization.inlineStyle)(hydra.serialization.spaceSep(elems))
    case hydra.lisp.syntax.Dialect.commonLisp => hydra.serialization.noSep(Seq(hydra.serialization.cst("#"),
       hydra.serialization.parens(hydra.serialization.spaceSep(elems))))
    case hydra.lisp.syntax.Dialect.scheme => hydra.serialization.noSep(Seq(hydra.serialization.cst("#"),
       hydra.serialization.parens(hydra.serialization.spaceSep(elems))))
}
