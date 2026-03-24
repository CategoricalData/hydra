package hydra.ext.haskell.serde

import hydra.ast.*

import hydra.ext.haskell.syntax.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maybes

import hydra.lib.strings

def alternativeToExpr(alt: hydra.ext.haskell.syntax.Alternative): hydra.ast.Expr =
  hydra.serialization.structuralSpaceSep(Seq(hydra.ext.haskell.serde.patternToExpr(alt.pattern), hydra.serialization.cst("->"), hydra.ext.haskell.serde.caseRhsToExpr(alt.rhs)))

def applicationExpressionToExpr(app: hydra.ext.haskell.syntax.ApplicationExpression): hydra.ast.Expr =
  hydra.serialization.ifx(hydra.ext.haskell.operators.appOp)(hydra.ext.haskell.serde.expressionToExpr(app.function))(hydra.ext.haskell.serde.expressionToExpr(app.argument))

def applicationPatternToExpr(appPat: hydra.ext.haskell.syntax.ApplicationPattern): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.haskell.syntax.Name = (appPat.name)
  lazy val pats: Seq[hydra.ext.haskell.syntax.Pattern] = (appPat.args)
  hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(hydra.lib.lists.map[hydra.ext.haskell.syntax.Pattern, hydra.ast.Expr](hydra.ext.haskell.serde.patternToExpr)(pats)))
}

def assertionToExpr(sert: hydra.ext.haskell.syntax.Assertion): hydra.ast.Expr =
  sert match
  case hydra.ext.haskell.syntax.Assertion.`class`(v_Assertion_class_cls) => hydra.ext.haskell.serde.classAssertionToExpr(v_Assertion_class_cls)
  case hydra.ext.haskell.syntax.Assertion.tuple(v_Assertion_tuple_serts) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.haskell.syntax.Assertion, hydra.ast.Expr](hydra.ext.haskell.serde.assertionToExpr)(v_Assertion_tuple_serts))

def caseExpressionToExpr(caseExpr: hydra.ext.haskell.syntax.CaseExpression): hydra.ast.Expr =
  {
  lazy val cs: hydra.ext.haskell.syntax.Expression = (caseExpr.`case`)
  lazy val alts: Seq[hydra.ext.haskell.syntax.Alternative] = (caseExpr.alternatives)
  lazy val ofOp: hydra.ast.Op = hydra.ast.Op("of", hydra.ast.Padding(hydra.ast.Ws.space, hydra.ast.Ws.breakAndIndent("  ")), 0, hydra.ast.Associativity.none)
  lazy val lhs: hydra.ast.Expr = hydra.serialization.spaceSep(Seq(hydra.serialization.cst("case"), hydra.ext.haskell.serde.expressionToExpr(cs)))
  lazy val rhs: hydra.ast.Expr = hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.haskell.syntax.Alternative, hydra.ast.Expr](hydra.ext.haskell.serde.alternativeToExpr)(alts))
  hydra.serialization.ifx(ofOp)(lhs)(rhs)
}

def caseRhsToExpr(rhs: hydra.ext.haskell.syntax.CaseRhs): hydra.ast.Expr = hydra.ext.haskell.serde.expressionToExpr(rhs)

def classAssertionToExpr(clsAsrt: hydra.ext.haskell.syntax.ClassAssertion): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.haskell.syntax.Name = (clsAsrt.name)
  lazy val types: Seq[hydra.ext.haskell.syntax.Type] = (clsAsrt.types)
  hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.commaSep(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.ext.haskell.syntax.Type, hydra.ast.Expr](hydra.ext.haskell.serde.typeToExpr)(types)))))
}

def constructorToExpr(cons: hydra.ext.haskell.syntax.Constructor): hydra.ast.Expr =
  cons match
  case hydra.ext.haskell.syntax.Constructor.ordinary(v_Constructor_ordinary_ord) => {
    lazy val name: hydra.ext.haskell.syntax.Name = (v_Constructor_ordinary_ord.name)
    lazy val types: Seq[hydra.ext.haskell.syntax.Type] = (v_Constructor_ordinary_ord.fields)
    hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.haskell.syntax.Type, hydra.ast.Expr](hydra.ext.haskell.serde.typeToExpr)(types)))))
  }
  case hydra.ext.haskell.syntax.Constructor.record(v_Constructor_record_rec) => {
    lazy val name: hydra.ext.haskell.syntax.Name = (v_Constructor_record_rec.name)
    lazy val fields: Seq[hydra.ext.haskell.syntax.FieldWithComments] = (v_Constructor_record_rec.fields)
    hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.curlyBracesList(None)(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.ext.haskell.syntax.FieldWithComments, hydra.ast.Expr](hydra.ext.haskell.serde.fieldWithCommentsToExpr)(fields)))))
  }

def constructorWithCommentsToExpr(consWithComments: hydra.ext.haskell.syntax.ConstructorWithComments): hydra.ast.Expr =
  {
  lazy val body: hydra.ext.haskell.syntax.Constructor = (consWithComments.body)
  lazy val mc: Option[scala.Predef.String] = (consWithComments.comments)
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](hydra.ext.haskell.serde.constructorToExpr(body))((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.ext.haskell.serde.toHaskellComments(c)))(Seq(hydra.ext.haskell.serde.constructorToExpr(body)))))(mc)
}

def dataOrNewtypeToExpr(kw: hydra.ext.haskell.syntax.DataOrNewtype): hydra.ast.Expr =
  kw match
  case hydra.ext.haskell.syntax.DataOrNewtype.data() => hydra.serialization.cst("data")
  case hydra.ext.haskell.syntax.DataOrNewtype.newtype() => hydra.serialization.cst("newtype")

def declarationHeadToExpr(hd: hydra.ext.haskell.syntax.DeclarationHead): hydra.ast.Expr =
  hd match
  case hydra.ext.haskell.syntax.DeclarationHead.application(v_DeclarationHead_application_appHead) => {
    lazy val fun: hydra.ext.haskell.syntax.DeclarationHead = (v_DeclarationHead_application_appHead.function)
    lazy val op: hydra.ext.haskell.syntax.Variable = (v_DeclarationHead_application_appHead.operand)
    hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.declarationHeadToExpr(fun))(Seq(hydra.ext.haskell.serde.variableToExpr(op))))
  }
  case hydra.ext.haskell.syntax.DeclarationHead.simple(v_DeclarationHead_simple_name) => hydra.ext.haskell.serde.nameToExpr(v_DeclarationHead_simple_name)

def declarationToExpr(decl: hydra.ext.haskell.syntax.Declaration): hydra.ast.Expr =
  decl match
  case hydra.ext.haskell.syntax.Declaration.data(v_Declaration_data_dataDecl) => {
    lazy val kw: hydra.ext.haskell.syntax.DataOrNewtype = (v_Declaration_data_dataDecl.keyword)
    lazy val hd: hydra.ext.haskell.syntax.DeclarationHead = (v_Declaration_data_dataDecl.head)
    lazy val cons: Seq[hydra.ext.haskell.syntax.ConstructorWithComments] = (v_Declaration_data_dataDecl.constructors)
    lazy val deriv: Seq[hydra.ext.haskell.syntax.Deriving] = (v_Declaration_data_dataDecl.deriving)
    lazy val derivCat: Seq[hydra.ext.haskell.syntax.Name] = hydra.lib.lists.concat[hydra.ext.haskell.syntax.Name](hydra.lib.lists.map[hydra.ext.haskell.syntax.Deriving, Seq[hydra.ext.haskell.syntax.Name]]((x) => x)(deriv))
    lazy val constructors: hydra.ast.Expr = hydra.serialization.orSep(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.ext.haskell.syntax.ConstructorWithComments, hydra.ast.Expr](hydra.ext.haskell.serde.constructorWithCommentsToExpr)(cons))
    lazy val derivingClause: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.haskell.syntax.Name](derivCat))(Seq())(Seq(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("deriving"))(Seq(hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.haskell.syntax.Name, hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr)(derivCat)))))))
    lazy val mainParts: Seq[hydra.ast.Expr] = Seq(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.dataOrNewtypeToExpr(kw))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.declarationHeadToExpr(hd))(Seq(hydra.serialization.cst("="))))), constructors)
    hydra.serialization.indentBlock(hydra.lib.lists.concat2[hydra.ast.Expr](mainParts)(derivingClause))
  }
  case hydra.ext.haskell.syntax.Declaration.`type`(v_Declaration_type_typeDecl) => {
    lazy val hd: hydra.ext.haskell.syntax.DeclarationHead = (v_Declaration_type_typeDecl.name)
    lazy val typ: hydra.ext.haskell.syntax.Type = (v_Declaration_type_typeDecl.`type`)
    hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("type"))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.declarationHeadToExpr(hd))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("="))(Seq(hydra.ext.haskell.serde.typeToExpr(typ))))))
  }
  case hydra.ext.haskell.syntax.Declaration.valueBinding(v_Declaration_valueBinding_vb) => hydra.ext.haskell.serde.valueBindingToExpr(v_Declaration_valueBinding_vb)
  case hydra.ext.haskell.syntax.Declaration.typedBinding(v_Declaration_typedBinding_typedBinding) => {
    lazy val typeSig: hydra.ext.haskell.syntax.TypeSignature = (v_Declaration_typedBinding_typedBinding.typeSignature)
    lazy val vb: hydra.ext.haskell.syntax.ValueBinding = (v_Declaration_typedBinding_typedBinding.valueBinding)
    lazy val name: hydra.ext.haskell.syntax.Name = (typeSig.name)
    lazy val htype: hydra.ext.haskell.syntax.Type = (typeSig.`type`)
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.structuralSpaceSep(Seq(hydra.ext.haskell.serde.nameToExpr(name), hydra.serialization.cst("::"), hydra.ext.haskell.serde.typeToExpr(htype))))(Seq(hydra.ext.haskell.serde.valueBindingToExpr(vb))))
  }

def declarationWithCommentsToExpr(declWithComments: hydra.ext.haskell.syntax.DeclarationWithComments): hydra.ast.Expr =
  {
  lazy val body: hydra.ext.haskell.syntax.Declaration = (declWithComments.body)
  lazy val mc: Option[scala.Predef.String] = (declWithComments.comments)
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](hydra.ext.haskell.serde.declarationToExpr(body))((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.ext.haskell.serde.toHaskellComments(c)))(Seq(hydra.ext.haskell.serde.declarationToExpr(body)))))(mc)
}

def expressionToExpr(expr: hydra.ext.haskell.syntax.Expression): hydra.ast.Expr =
  expr match
  case hydra.ext.haskell.syntax.Expression.application(v_Expression_application_app) => hydra.ext.haskell.serde.applicationExpressionToExpr(v_Expression_application_app)
  case hydra.ext.haskell.syntax.Expression.`case`(v_Expression_case_cases) => hydra.ext.haskell.serde.caseExpressionToExpr(v_Expression_case_cases)
  case hydra.ext.haskell.syntax.Expression.constructRecord(v_Expression_constructRecord_r) => hydra.ext.haskell.serde.constructRecordExpressionToExpr(v_Expression_constructRecord_r)
  case hydra.ext.haskell.syntax.Expression.`do`(v_Expression_do_statements) => hydra.serialization.indentBlock(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("do"))(hydra.lib.lists.map[hydra.ext.haskell.syntax.Statement, hydra.ast.Expr](hydra.ext.haskell.serde.statementToExpr)(v_Expression_do_statements)))
  case hydra.ext.haskell.syntax.Expression.`if`(v_Expression_if_ifte) => hydra.ext.haskell.serde.ifExpressionToExpr(v_Expression_if_ifte)
  case hydra.ext.haskell.syntax.Expression.literal(v_Expression_literal_lit) => hydra.ext.haskell.serde.literalToExpr(v_Expression_literal_lit)
  case hydra.ext.haskell.syntax.Expression.lambda(v_Expression_lambda_lam) => hydra.serialization.parenthesize(hydra.ext.haskell.serde.lambdaExpressionToExpr(v_Expression_lambda_lam))
  case hydra.ext.haskell.syntax.Expression.let(v_Expression_let_letExpr) => {
    lazy val bindings: Seq[hydra.ext.haskell.syntax.LocalBinding] = (v_Expression_let_letExpr.bindings)
    lazy val inner: hydra.ext.haskell.syntax.Expression = (v_Expression_let_letExpr.inner)
    def encodeBinding(binding: hydra.ext.haskell.syntax.LocalBinding): hydra.ast.Expr =
      hydra.serialization.indentSubsequentLines("    ")(hydra.ext.haskell.serde.localBindingToExpr(binding))
    hydra.serialization.indentBlock(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(""))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("let"))(Seq(hydra.serialization.customIndentBlock("    ")(hydra.lib.lists.map[hydra.ext.haskell.syntax.LocalBinding, hydra.ast.Expr](encodeBinding)(bindings))))))(Seq(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("in"))(Seq(hydra.ext.haskell.serde.expressionToExpr(inner))))))))
  }
  case hydra.ext.haskell.syntax.Expression.list(v_Expression_list_exprs) => hydra.serialization.bracketList(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.ext.haskell.syntax.Expression, hydra.ast.Expr](hydra.ext.haskell.serde.expressionToExpr)(v_Expression_list_exprs))
  case hydra.ext.haskell.syntax.Expression.parens(v_Expression_parens_expr_) => hydra.serialization.parenthesize(hydra.ext.haskell.serde.expressionToExpr(`v_Expression_parens_expr_`))
  case hydra.ext.haskell.syntax.Expression.tuple(v_Expression_tuple_exprs) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.haskell.syntax.Expression, hydra.ast.Expr](hydra.ext.haskell.serde.expressionToExpr)(v_Expression_tuple_exprs))
  case hydra.ext.haskell.syntax.Expression.variable(v_Expression_variable_name) => hydra.ext.haskell.serde.nameToExpr(v_Expression_variable_name)

def constructRecordExpressionToExpr(constructRecord: hydra.ext.haskell.syntax.ConstructRecordExpression): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.haskell.syntax.Name = (constructRecord.name)
  lazy val updates: Seq[hydra.ext.haskell.syntax.FieldUpdate] = (constructRecord.fields)
  def fromUpdate(update: hydra.ext.haskell.syntax.FieldUpdate): hydra.ast.Expr =
    {
    lazy val fn: hydra.ext.haskell.syntax.Name = (update.name)
    lazy val `val`: hydra.ext.haskell.syntax.Expression = (update.value)
    hydra.serialization.ifx(hydra.ext.haskell.operators.defineOp)(hydra.ext.haskell.serde.nameToExpr(fn))(hydra.ext.haskell.serde.expressionToExpr(`val`))
  }
  lazy val body: hydra.ast.Expr = hydra.serialization.commaSep(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.ext.haskell.syntax.FieldUpdate, hydra.ast.Expr](fromUpdate)(updates))
  hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.brackets(hydra.serialization.curlyBraces)(hydra.serialization.halfBlockStyle)(body))))
}

def fieldToExpr(field: hydra.ext.haskell.syntax.Field): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.haskell.syntax.Name = (field.name)
  lazy val typ: hydra.ext.haskell.syntax.Type = (field.`type`)
  hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("::"))(Seq(hydra.ext.haskell.serde.typeToExpr(typ)))))
}

def fieldWithCommentsToExpr(fieldWithComments: hydra.ext.haskell.syntax.FieldWithComments): hydra.ast.Expr =
  {
  lazy val field: hydra.ext.haskell.syntax.Field = (fieldWithComments.field)
  lazy val mc: Option[scala.Predef.String] = (fieldWithComments.comments)
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](hydra.ext.haskell.serde.fieldToExpr(field))((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.ext.haskell.serde.toHaskellComments(c)))(Seq(hydra.ext.haskell.serde.fieldToExpr(field)))))(mc)
}

def ifExpressionToExpr(ifExpr: hydra.ext.haskell.syntax.IfExpression): hydra.ast.Expr =
  {
  lazy val eif: hydra.ext.haskell.syntax.Expression = (ifExpr.condition)
  lazy val ethen: hydra.ext.haskell.syntax.Expression = (ifExpr.`then`)
  lazy val eelse: hydra.ext.haskell.syntax.Expression = (ifExpr.`else`)
  lazy val ifOp: hydra.ast.Op = hydra.ast.Op("", hydra.ast.Padding(hydra.ast.Ws.none, hydra.ast.Ws.breakAndIndent("  ")), 0, hydra.ast.Associativity.none)
  lazy val body: hydra.ast.Expr = hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("then"))(Seq(hydra.ext.haskell.serde.expressionToExpr(ethen)))))(Seq(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("else"))(Seq(hydra.ext.haskell.serde.expressionToExpr(eelse)))))))
  hydra.serialization.ifx(ifOp)(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("if"))(Seq(hydra.ext.haskell.serde.expressionToExpr(eif)))))(body)
}

def importExportSpecToExpr(spec: hydra.ext.haskell.syntax.ImportExportSpec): hydra.ast.Expr = hydra.ext.haskell.serde.nameToExpr(spec.name)

def importToExpr(`import`: hydra.ext.haskell.syntax.Import): hydra.ast.Expr =
  {
  lazy val qual: Boolean = (`import`.qualified)
  lazy val modName: hydra.ext.haskell.syntax.ModuleName = (`import`.module)
  lazy val mod: Option[hydra.ext.haskell.syntax.ModuleName] = (`import`.as)
  lazy val mspec: Option[hydra.ext.haskell.syntax.SpecImport] = (`import`.spec)
  lazy val name: scala.Predef.String = modName
  def hidingSec(spec: hydra.ext.haskell.syntax.SpecImport): hydra.ast.Expr =
    spec match
    case hydra.ext.haskell.syntax.SpecImport.hiding(v_SpecImport_hiding_names) => hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("hiding "))(Seq(hydra.serialization.parens(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.haskell.syntax.ImportExportSpec, hydra.ast.Expr](hydra.ext.haskell.serde.importExportSpecToExpr)(v_SpecImport_hiding_names))))))
  lazy val parts: Seq[hydra.ast.Expr] = hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("import")), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](qual)(Some(hydra.serialization.cst("qualified")))(None), Some(hydra.serialization.cst(name)), hydra.lib.maybes.map[hydra.ext.haskell.syntax.ModuleName, hydra.ast.Expr]((m: hydra.ext.haskell.syntax.ModuleName) => hydra.serialization.cst(hydra.lib.strings.cat2("as ")(m)))(mod), hydra.lib.maybes.map[hydra.ext.haskell.syntax.SpecImport, hydra.ast.Expr](hidingSec)(mspec)))
  hydra.serialization.spaceSep(parts)
}

def lambdaExpressionToExpr(lambdaExpr: hydra.ext.haskell.syntax.LambdaExpression): hydra.ast.Expr =
  {
  lazy val bindings: Seq[hydra.ext.haskell.syntax.Pattern] = (lambdaExpr.bindings)
  lazy val inner: hydra.ext.haskell.syntax.Expression = (lambdaExpr.inner)
  lazy val head: hydra.ast.Expr = hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.haskell.syntax.Pattern, hydra.ast.Expr](hydra.ext.haskell.serde.patternToExpr)(bindings))
  lazy val body: hydra.ast.Expr = hydra.ext.haskell.serde.expressionToExpr(inner)
  hydra.serialization.ifx(hydra.ext.haskell.operators.lambdaOp)(hydra.serialization.prefix("\\")(head))(body)
}

def literalToExpr(lit: hydra.ext.haskell.syntax.Literal): hydra.ast.Expr =
  {
  def parensIfNeg(b: Boolean)(e: scala.Predef.String): scala.Predef.String =
    hydra.lib.logic.ifElse[scala.Predef.String](b)(hydra.lib.strings.cat(Seq("(", e, ")")))(e)
  hydra.serialization.cst(lit match
    case hydra.ext.haskell.syntax.Literal.char(v_Literal_char_c) => hydra.lib.literals.showString(hydra.lib.literals.showUint16(v_Literal_char_c))
    case hydra.ext.haskell.syntax.Literal.double(v_Literal_double_d) => parensIfNeg(hydra.lib.equality.lt[Double](v_Literal_double_d)(0.0))(hydra.lib.literals.showFloat64(v_Literal_double_d))
    case hydra.ext.haskell.syntax.Literal.float(v_Literal_float_f) => parensIfNeg(hydra.lib.equality.lt[Float](v_Literal_float_f)(0.0f))(hydra.lib.literals.showFloat32(v_Literal_float_f))
    case hydra.ext.haskell.syntax.Literal.int(v_Literal_int_i) => parensIfNeg(hydra.lib.equality.lt[Int](v_Literal_int_i)(0))(hydra.lib.literals.showInt32(v_Literal_int_i))
    case hydra.ext.haskell.syntax.Literal.integer(v_Literal_integer_i) => parensIfNeg(hydra.lib.equality.lt[BigInt](v_Literal_integer_i)(BigInt(0L)))(hydra.lib.literals.showBigint(v_Literal_integer_i))
    case hydra.ext.haskell.syntax.Literal.string(v_Literal_string_s) => hydra.lib.literals.showString(v_Literal_string_s))
}

def localBindingToExpr(binding: hydra.ext.haskell.syntax.LocalBinding): hydra.ast.Expr =
  binding match
  case hydra.ext.haskell.syntax.LocalBinding.signature(v_LocalBinding_signature_ts) => hydra.ext.haskell.serde.typeSignatureToExpr(v_LocalBinding_signature_ts)
  case hydra.ext.haskell.syntax.LocalBinding.value(v_LocalBinding_value_vb) => hydra.ext.haskell.serde.valueBindingToExpr(v_LocalBinding_value_vb)

def moduleHeadToExpr(moduleHead: hydra.ext.haskell.syntax.ModuleHead): hydra.ast.Expr =
  {
  lazy val mc: Option[scala.Predef.String] = (moduleHead.comments)
  lazy val modName: hydra.ext.haskell.syntax.ModuleName = (moduleHead.name)
  lazy val mname: scala.Predef.String = modName
  lazy val head: hydra.ast.Expr = hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("module"))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(mname))(Seq(hydra.serialization.cst("where")))))
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](head)((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.ext.haskell.serde.toHaskellComments(c)))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(""))(Seq(head)))))(mc)
}

def moduleToExpr(module: hydra.ext.haskell.syntax.Module): hydra.ast.Expr =
  {
  lazy val mh: Option[hydra.ext.haskell.syntax.ModuleHead] = (module.head)
  lazy val imports: Seq[hydra.ext.haskell.syntax.Import] = (module.imports)
  lazy val decls: Seq[hydra.ext.haskell.syntax.DeclarationWithComments] = (module.declarations)
  lazy val warning: Seq[hydra.ast.Expr] = Seq(hydra.serialization.cst(hydra.ext.haskell.serde.toSimpleComments(hydra.constants.warningAutoGeneratedFile)))
  lazy val headerLine: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.haskell.syntax.ModuleHead](Seq())((h: hydra.ext.haskell.syntax.ModuleHead) => Seq(hydra.ext.haskell.serde.moduleHeadToExpr(h)))(mh)
  lazy val declLines: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.ext.haskell.syntax.DeclarationWithComments, hydra.ast.Expr](hydra.ext.haskell.serde.declarationWithCommentsToExpr)(decls)
  lazy val importLines: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.haskell.syntax.Import](imports))(Seq())(Seq(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.haskell.syntax.Import, hydra.ast.Expr](hydra.ext.haskell.serde.importToExpr)(imports))))
  hydra.serialization.doubleNewlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(warning, headerLine, importLines, declLines)))
}

def nameToExpr(name: hydra.ext.haskell.syntax.Name): hydra.ast.Expr =
  hydra.serialization.cst(name match
  case hydra.ext.haskell.syntax.Name.`implicit`(v_Name_implicit_qn) => hydra.lib.strings.cat2("?")(hydra.ext.haskell.serde.writeQualifiedName(v_Name_implicit_qn))
  case hydra.ext.haskell.syntax.Name.normal(v_Name_normal_qn) => hydra.ext.haskell.serde.writeQualifiedName(v_Name_normal_qn)
  case hydra.ext.haskell.syntax.Name.parens(v_Name_parens_qn) => hydra.lib.strings.cat(Seq("(", hydra.ext.haskell.serde.writeQualifiedName(v_Name_parens_qn), ")")))

def patternToExpr(pat: hydra.ext.haskell.syntax.Pattern): hydra.ast.Expr =
  pat match
  case hydra.ext.haskell.syntax.Pattern.application(v_Pattern_application_app) => hydra.ext.haskell.serde.applicationPatternToExpr(v_Pattern_application_app)
  case hydra.ext.haskell.syntax.Pattern.list(v_Pattern_list_pats) => hydra.serialization.bracketList(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.ext.haskell.syntax.Pattern, hydra.ast.Expr](hydra.ext.haskell.serde.patternToExpr)(v_Pattern_list_pats))
  case hydra.ext.haskell.syntax.Pattern.literal(v_Pattern_literal_lit) => hydra.ext.haskell.serde.literalToExpr(v_Pattern_literal_lit)
  case hydra.ext.haskell.syntax.Pattern.name(v_Pattern_name_name) => hydra.ext.haskell.serde.nameToExpr(v_Pattern_name_name)
  case hydra.ext.haskell.syntax.Pattern.parens(v_Pattern_parens_pat_) => hydra.serialization.parenthesize(hydra.ext.haskell.serde.patternToExpr(`v_Pattern_parens_pat_`))
  case hydra.ext.haskell.syntax.Pattern.tuple(v_Pattern_tuple_pats) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.haskell.syntax.Pattern, hydra.ast.Expr](hydra.ext.haskell.serde.patternToExpr)(v_Pattern_tuple_pats))
  case hydra.ext.haskell.syntax.Pattern.wildcard() => hydra.serialization.cst("_")

def rightHandSideToExpr(rhs: hydra.ext.haskell.syntax.RightHandSide): hydra.ast.Expr = hydra.ext.haskell.serde.expressionToExpr(rhs)

def statementToExpr(stmt: hydra.ext.haskell.syntax.Statement): hydra.ast.Expr = hydra.ext.haskell.serde.expressionToExpr(stmt)

def typeSignatureToExpr(typeSig: hydra.ext.haskell.syntax.TypeSignature): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.haskell.syntax.Name = (typeSig.name)
  lazy val typ: hydra.ext.haskell.syntax.Type = (typeSig.`type`)
  lazy val nameExpr: hydra.ast.Expr = hydra.ext.haskell.serde.nameToExpr(name)
  lazy val typeExpr: hydra.ast.Expr = hydra.ext.haskell.serde.typeToExpr(typ)
  lazy val inlineSig: hydra.ast.Expr = hydra.serialization.structuralSpaceSep(Seq(nameExpr, hydra.serialization.cst("::"), typeExpr))
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.gt[Int](hydra.serialization.expressionLength(inlineSig))(120))(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(nameExpr, hydra.serialization.cst("::"))), hydra.serialization.tabIndent(typeExpr))))(inlineSig)
}

def typeToExpr(htype: hydra.ext.haskell.syntax.Type): hydra.ast.Expr =
  htype match
  case hydra.ext.haskell.syntax.Type.application(v_Type_application_appType) => {
    lazy val lhs: hydra.ext.haskell.syntax.Type = (v_Type_application_appType.context)
    lazy val rhs: hydra.ext.haskell.syntax.Type = (v_Type_application_appType.argument)
    hydra.serialization.ifx(hydra.ext.haskell.operators.appOp)(hydra.ext.haskell.serde.typeToExpr(lhs))(hydra.ext.haskell.serde.typeToExpr(rhs))
  }
  case hydra.ext.haskell.syntax.Type.ctx(v_Type_ctx_ctxType) => {
    lazy val ctx: hydra.ext.haskell.syntax.Assertion = (v_Type_ctx_ctxType.ctx)
    lazy val typ: hydra.ext.haskell.syntax.Type = (v_Type_ctx_ctxType.`type`)
    hydra.serialization.ifx(hydra.ext.haskell.operators.assertOp)(hydra.ext.haskell.serde.assertionToExpr(ctx))(hydra.ext.haskell.serde.typeToExpr(typ))
  }
  case hydra.ext.haskell.syntax.Type.function(v_Type_function_funType) => {
    lazy val dom: hydra.ext.haskell.syntax.Type = (v_Type_function_funType.domain)
    lazy val cod: hydra.ext.haskell.syntax.Type = (v_Type_function_funType.codomain)
    hydra.serialization.ifx(hydra.ext.haskell.operators.arrowOp)(hydra.ext.haskell.serde.typeToExpr(dom))(hydra.ext.haskell.serde.typeToExpr(cod))
  }
  case hydra.ext.haskell.syntax.Type.list(v_Type_list_htype_) => hydra.serialization.bracketList(hydra.serialization.inlineStyle)(Seq(hydra.ext.haskell.serde.typeToExpr(`v_Type_list_htype_`)))
  case hydra.ext.haskell.syntax.Type.tuple(v_Type_tuple_types) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.haskell.syntax.Type, hydra.ast.Expr](hydra.ext.haskell.serde.typeToExpr)(v_Type_tuple_types))
  case hydra.ext.haskell.syntax.Type.variable(v_Type_variable_name) => hydra.ext.haskell.serde.nameToExpr(v_Type_variable_name)

def valueBindingToExpr(vb: hydra.ext.haskell.syntax.ValueBinding): hydra.ast.Expr =
  vb match
  case hydra.ext.haskell.syntax.ValueBinding.simple(v_ValueBinding_simple_simpleVB) => {
    lazy val pat: hydra.ext.haskell.syntax.Pattern = (v_ValueBinding_simple_simpleVB.pattern)
    lazy val rhs: hydra.ext.haskell.syntax.RightHandSide = (v_ValueBinding_simple_simpleVB.rhs)
    lazy val local: Option[hydra.ext.haskell.syntax.LocalBindings] = (v_ValueBinding_simple_simpleVB.localBindings)
    lazy val lhsExpr: hydra.ast.Expr = hydra.ext.haskell.serde.patternToExpr(pat)
    lazy val rhsExpr: hydra.ast.Expr = hydra.ext.haskell.serde.rightHandSideToExpr(rhs)
    lazy val inlineBody: hydra.ast.Expr = hydra.serialization.structuralSpaceSep(Seq(lhsExpr, hydra.serialization.cst("="), rhsExpr))
    lazy val body: hydra.ast.Expr = hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.gt[Int](hydra.serialization.expressionLength(inlineBody))(120))(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(lhsExpr, hydra.serialization.cst("="))), hydra.serialization.tabIndent(rhsExpr))))(inlineBody)
    hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.haskell.syntax.LocalBindings](body)((localBindings: hydra.ext.haskell.syntax.LocalBindings) =>
      {
      lazy val bindings: Seq[hydra.ext.haskell.syntax.LocalBinding] = localBindings
      hydra.serialization.indentBlock(hydra.lib.lists.cons[hydra.ast.Expr](body)(Seq(hydra.serialization.indentBlock(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("where"))(hydra.lib.lists.map[hydra.ext.haskell.syntax.LocalBinding, hydra.ast.Expr](hydra.ext.haskell.serde.localBindingToExpr)(bindings))))))
    })(local)
  }

def variableToExpr(variable: hydra.ext.haskell.syntax.Variable): hydra.ast.Expr = hydra.ext.haskell.serde.nameToExpr(variable)

def toHaskellComments(c: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.intercalate("\n")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => hydra.lib.strings.cat2("-- | ")(s))(hydra.lib.strings.lines(c)))

def toSimpleComments(c: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.intercalate("\n")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => hydra.lib.strings.cat2("-- ")(s))(hydra.lib.strings.lines(c)))

def writeQualifiedName(qname: hydra.ext.haskell.syntax.QualifiedName): scala.Predef.String =
  {
  lazy val qualifiers: Seq[hydra.ext.haskell.syntax.NamePart] = (qname.qualifiers)
  lazy val unqual: hydra.ext.haskell.syntax.NamePart = (qname.unqualified)
  def h(namePart: hydra.ext.haskell.syntax.NamePart): scala.Predef.String = namePart
  lazy val allParts: Seq[scala.Predef.String] = hydra.lib.lists.concat2[scala.Predef.String](hydra.lib.lists.map[hydra.ext.haskell.syntax.NamePart, scala.Predef.String](h)(qualifiers))(Seq(h(unqual)))
  hydra.lib.strings.intercalate(".")(allParts)
}
