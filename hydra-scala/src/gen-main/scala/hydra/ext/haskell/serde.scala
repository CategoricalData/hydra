package hydra.ext.haskell.serde

import hydra.ast.*

import hydra.ext.haskell.ast.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maybes

import hydra.lib.strings

def alternativeToExpr(alt: hydra.ext.haskell.ast.Alternative): hydra.ast.Expr =
  hydra.serialization.structuralSpaceSep(Seq(hydra.ext.haskell.serde.patternToExpr(alt.pattern), hydra.serialization.cst("->"),
     hydra.ext.haskell.serde.caseRhsToExpr(alt.rhs)))

def applicationExpressionToExpr(app: hydra.ext.haskell.ast.ApplicationExpression): hydra.ast.Expr =
  hydra.serialization.ifx(hydra.ext.haskell.operators.appOp)(hydra.ext.haskell.serde.expressionToExpr(app.function))(hydra.ext.haskell.serde.expressionToExpr(app.argument))

def applicationPatternToExpr(appPat: hydra.ext.haskell.ast.ApplicationPattern): hydra.ast.Expr =
  {
  val name: hydra.ext.haskell.ast.Name = (appPat.name)
  val pats: Seq[hydra.ext.haskell.ast.Pattern] = (appPat.args)
  hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(lists.map[hydra.ext.haskell.ast.Pattern,
     hydra.ast.Expr](hydra.ext.haskell.serde.patternToExpr)(pats)))
}

def assertionToExpr(sert: hydra.ext.haskell.ast.Assertion): hydra.ast.Expr =
  sert match
  case hydra.ext.haskell.ast.Assertion.`class`(v_Assertion_class_cls) => hydra.ext.haskell.serde.classAssertionToExpr(v_Assertion_class_cls)
  case hydra.ext.haskell.ast.Assertion.tuple(v_Assertion_tuple_serts) => hydra.serialization.parenList(false)(lists.map[hydra.ext.haskell.ast.Assertion,
     hydra.ast.Expr](hydra.ext.haskell.serde.assertionToExpr)(v_Assertion_tuple_serts))

def caseExpressionToExpr(caseExpr: hydra.ext.haskell.ast.CaseExpression): hydra.ast.Expr =
  {
  val cs: hydra.ext.haskell.ast.Expression = (caseExpr.`case`)
  val alts: Seq[hydra.ext.haskell.ast.Alternative] = (caseExpr.alternatives)
  val ofOp: hydra.ast.Op = hydra.ast.Op("of", hydra.ast.Padding(hydra.ast.Ws.space, hydra.ast.Ws.breakAndIndent("  ")), 0, hydra.ast.Associativity.none)
  val lhs: hydra.ast.Expr = hydra.serialization.spaceSep(Seq(hydra.serialization.cst("case"), hydra.ext.haskell.serde.expressionToExpr(cs)))
  val rhs: hydra.ast.Expr = hydra.serialization.newlineSep(lists.map[hydra.ext.haskell.ast.Alternative,
     hydra.ast.Expr](hydra.ext.haskell.serde.alternativeToExpr)(alts))
  hydra.serialization.ifx(ofOp)(lhs)(rhs)
}

def caseRhsToExpr(rhs: hydra.ext.haskell.ast.CaseRhs): hydra.ast.Expr = hydra.ext.haskell.serde.expressionToExpr(rhs)

def classAssertionToExpr(clsAsrt: hydra.ext.haskell.ast.ClassAssertion): hydra.ast.Expr =
  {
  val name: hydra.ext.haskell.ast.Name = (clsAsrt.name)
  val types: Seq[hydra.ext.haskell.ast.Type] = (clsAsrt.types)
  hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.commaSep(hydra.serialization.halfBlockStyle)(lists.map[hydra.ext.haskell.ast.Type,
     hydra.ast.Expr](hydra.ext.haskell.serde.typeToExpr)(types)))))
}

def constructorToExpr(cons: hydra.ext.haskell.ast.Constructor): hydra.ast.Expr =
  cons match
  case hydra.ext.haskell.ast.Constructor.ordinary(v_Constructor_ordinary_ord) => {
    val name: hydra.ext.haskell.ast.Name = (v_Constructor_ordinary_ord.name)
    val types: Seq[hydra.ext.haskell.ast.Type] = (v_Constructor_ordinary_ord.fields)
    hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.spaceSep(lists.map[hydra.ext.haskell.ast.Type,
       hydra.ast.Expr](hydra.ext.haskell.serde.typeToExpr)(types)))))
  }
  case hydra.ext.haskell.ast.Constructor.record(v_Constructor_record_rec) => {
    val name: hydra.ext.haskell.ast.Name = (v_Constructor_record_rec.name)
    val fields: Seq[hydra.ext.haskell.ast.FieldWithComments] = (v_Constructor_record_rec.fields)
    hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.curlyBracesList(None)(hydra.serialization.halfBlockStyle)(lists.map[hydra.ext.haskell.ast.FieldWithComments,
       hydra.ast.Expr](hydra.ext.haskell.serde.fieldWithCommentsToExpr)(fields)))))
  }

def constructorWithCommentsToExpr(consWithComments: hydra.ext.haskell.ast.ConstructorWithComments): hydra.ast.Expr =
  {
  val body: hydra.ext.haskell.ast.Constructor = (consWithComments.body)
  val mc: Option[scala.Predef.String] = (consWithComments.comments)
  maybes.maybe[hydra.ast.Expr, scala.Predef.String](hydra.ext.haskell.serde.constructorToExpr(body))((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.ext.haskell.serde.toHaskellComments(c)))(Seq(hydra.ext.haskell.serde.constructorToExpr(body)))))(mc)
}

def dataOrNewtypeToExpr(kw: hydra.ext.haskell.ast.DataOrNewtype): hydra.ast.Expr =
  kw match
  case hydra.ext.haskell.ast.DataOrNewtype.data => hydra.serialization.cst("data")
  case hydra.ext.haskell.ast.DataOrNewtype.newtype => hydra.serialization.cst("newtype")

def declarationHeadToExpr(hd: hydra.ext.haskell.ast.DeclarationHead): hydra.ast.Expr =
  hd match
  case hydra.ext.haskell.ast.DeclarationHead.application(v_DeclarationHead_application_appHead) => {
    val fun: hydra.ext.haskell.ast.DeclarationHead = (v_DeclarationHead_application_appHead.function)
    val op: hydra.ext.haskell.ast.Variable = (v_DeclarationHead_application_appHead.operand)
    hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.declarationHeadToExpr(fun))(Seq(hydra.ext.haskell.serde.variableToExpr(op))))
  }
  case hydra.ext.haskell.ast.DeclarationHead.simple(v_DeclarationHead_simple_name) => hydra.ext.haskell.serde.nameToExpr(v_DeclarationHead_simple_name)

def declarationToExpr(decl: hydra.ext.haskell.ast.Declaration): hydra.ast.Expr =
  decl match
  case hydra.ext.haskell.ast.Declaration.data(v_Declaration_data_dataDecl) => {
    val kw: hydra.ext.haskell.ast.DataOrNewtype = (v_Declaration_data_dataDecl.keyword)
    val hd: hydra.ext.haskell.ast.DeclarationHead = (v_Declaration_data_dataDecl.head)
    val cons: Seq[hydra.ext.haskell.ast.ConstructorWithComments] = (v_Declaration_data_dataDecl.constructors)
    val deriv: Seq[hydra.ext.haskell.ast.Deriving] = (v_Declaration_data_dataDecl.deriving)
    val derivCat: Seq[hydra.ext.haskell.ast.Name] = lists.concat[hydra.ext.haskell.ast.Name](lists.map[hydra.ext.haskell.ast.Deriving,
       Seq[hydra.ext.haskell.ast.Name]]((x) => x)(deriv))
    val constructors: hydra.ast.Expr = hydra.serialization.orSep(hydra.serialization.halfBlockStyle)(lists.map[hydra.ext.haskell.ast.ConstructorWithComments,
       hydra.ast.Expr](hydra.ext.haskell.serde.constructorWithCommentsToExpr)(cons))
    val derivingClause: Seq[hydra.ast.Expr] = logic.ifElse[Seq[hydra.ast.Expr]](lists.`null`[hydra.ext.haskell.ast.Name](derivCat))(Seq())(Seq(hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst("deriving"))(Seq(hydra.serialization.parenList(false)(lists.map[hydra.ext.haskell.ast.Name,
       hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr)(derivCat)))))))
    val mainParts: Seq[hydra.ast.Expr] = Seq(hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.dataOrNewtypeToExpr(kw))(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.declarationHeadToExpr(hd))(Seq(hydra.serialization.cst("="))))),
       constructors)
    hydra.serialization.indentBlock(lists.concat2[hydra.ast.Expr](mainParts)(derivingClause))
  }
  case hydra.ext.haskell.ast.Declaration.`type`(v_Declaration_type_typeDecl) => {
    val hd: hydra.ext.haskell.ast.DeclarationHead = (v_Declaration_type_typeDecl.name)
    val typ: hydra.ext.haskell.ast.Type = (v_Declaration_type_typeDecl.`type`)
    hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst("type"))(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.declarationHeadToExpr(hd))(lists.cons[hydra.ast.Expr](hydra.serialization.cst("="))(Seq(hydra.ext.haskell.serde.typeToExpr(typ))))))
  }
  case hydra.ext.haskell.ast.Declaration.valueBinding(v_Declaration_valueBinding_vb) => hydra.ext.haskell.serde.valueBindingToExpr(v_Declaration_valueBinding_vb)
  case hydra.ext.haskell.ast.Declaration.typedBinding(v_Declaration_typedBinding_typedBinding) => {
    val typeSig: hydra.ext.haskell.ast.TypeSignature = (v_Declaration_typedBinding_typedBinding.typeSignature)
    val vb: hydra.ext.haskell.ast.ValueBinding = (v_Declaration_typedBinding_typedBinding.valueBinding)
    val name: hydra.ext.haskell.ast.Name = (typeSig.name)
    val htype: hydra.ext.haskell.ast.Type = (typeSig.`type`)
    hydra.serialization.newlineSep(lists.cons[hydra.ast.Expr](hydra.serialization.structuralSpaceSep(Seq(hydra.ext.haskell.serde.nameToExpr(name),
       hydra.serialization.cst("::"), hydra.ext.haskell.serde.typeToExpr(htype))))(Seq(hydra.ext.haskell.serde.valueBindingToExpr(vb))))
  }

def declarationWithCommentsToExpr(declWithComments: hydra.ext.haskell.ast.DeclarationWithComments): hydra.ast.Expr =
  {
  val body: hydra.ext.haskell.ast.Declaration = (declWithComments.body)
  val mc: Option[scala.Predef.String] = (declWithComments.comments)
  maybes.maybe[hydra.ast.Expr, scala.Predef.String](hydra.ext.haskell.serde.declarationToExpr(body))((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.ext.haskell.serde.toHaskellComments(c)))(Seq(hydra.ext.haskell.serde.declarationToExpr(body)))))(mc)
}

def expressionToExpr(expr: hydra.ext.haskell.ast.Expression): hydra.ast.Expr =
  expr match
  case hydra.ext.haskell.ast.Expression.application(v_Expression_application_app) => hydra.ext.haskell.serde.applicationExpressionToExpr(v_Expression_application_app)
  case hydra.ext.haskell.ast.Expression.`case`(v_Expression_case_cases) => hydra.ext.haskell.serde.caseExpressionToExpr(v_Expression_case_cases)
  case hydra.ext.haskell.ast.Expression.constructRecord(v_Expression_constructRecord_r) => hydra.ext.haskell.serde.constructRecordExpressionToExpr(v_Expression_constructRecord_r)
  case hydra.ext.haskell.ast.Expression.`do`(v_Expression_do_statements) => hydra.serialization.indentBlock(lists.cons[hydra.ast.Expr](hydra.serialization.cst("do"))(lists.map[hydra.ext.haskell.ast.Statement,
     hydra.ast.Expr](hydra.ext.haskell.serde.statementToExpr)(v_Expression_do_statements)))
  case hydra.ext.haskell.ast.Expression.`if`(v_Expression_if_ifte) => hydra.ext.haskell.serde.ifExpressionToExpr(v_Expression_if_ifte)
  case hydra.ext.haskell.ast.Expression.literal(v_Expression_literal_lit) => hydra.ext.haskell.serde.literalToExpr(v_Expression_literal_lit)
  case hydra.ext.haskell.ast.Expression.lambda(v_Expression_lambda_lam) => hydra.serialization.parenthesize(hydra.ext.haskell.serde.lambdaExpressionToExpr(v_Expression_lambda_lam))
  case hydra.ext.haskell.ast.Expression.let(v_Expression_let_letExpr) => {
    val bindings: Seq[hydra.ext.haskell.ast.LocalBinding] = (v_Expression_let_letExpr.bindings)
    val inner: hydra.ext.haskell.ast.Expression = (v_Expression_let_letExpr.inner)
    def encodeBinding(binding: hydra.ext.haskell.ast.LocalBinding): hydra.ast.Expr =
      hydra.serialization.indentSubsequentLines("    ")(hydra.ext.haskell.serde.localBindingToExpr(binding))
    hydra.serialization.indentBlock(lists.cons[hydra.ast.Expr](hydra.serialization.cst(""))(lists.cons[hydra.ast.Expr](hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst("let"))(Seq(hydra.serialization.customIndentBlock("    ")(lists.map[hydra.ext.haskell.ast.LocalBinding,
       hydra.ast.Expr](encodeBinding)(bindings))))))(Seq(hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst("in"))(Seq(hydra.ext.haskell.serde.expressionToExpr(inner))))))))
  }
  case hydra.ext.haskell.ast.Expression.list(v_Expression_list_exprs) => hydra.serialization.bracketList(hydra.serialization.halfBlockStyle)(lists.map[hydra.ext.haskell.ast.Expression,
     hydra.ast.Expr](hydra.ext.haskell.serde.expressionToExpr)(v_Expression_list_exprs))
  case hydra.ext.haskell.ast.Expression.parens(v_Expression_parens_expr_) => hydra.serialization.parenthesize(hydra.ext.haskell.serde.expressionToExpr(`v_Expression_parens_expr_`))
  case hydra.ext.haskell.ast.Expression.tuple(v_Expression_tuple_exprs) => hydra.serialization.parenList(false)(lists.map[hydra.ext.haskell.ast.Expression,
     hydra.ast.Expr](hydra.ext.haskell.serde.expressionToExpr)(v_Expression_tuple_exprs))
  case hydra.ext.haskell.ast.Expression.variable(v_Expression_variable_name) => hydra.ext.haskell.serde.nameToExpr(v_Expression_variable_name)

def constructRecordExpressionToExpr(constructRecord: hydra.ext.haskell.ast.ConstructRecordExpression): hydra.ast.Expr =
  {
  val name: hydra.ext.haskell.ast.Name = (constructRecord.name)
  val updates: Seq[hydra.ext.haskell.ast.FieldUpdate] = (constructRecord.fields)
  def fromUpdate(update: hydra.ext.haskell.ast.FieldUpdate): hydra.ast.Expr =
    {
    val fn: hydra.ext.haskell.ast.Name = (update.name)
    val `val`: hydra.ext.haskell.ast.Expression = (update.value)
    hydra.serialization.ifx(hydra.ext.haskell.operators.defineOp)(hydra.ext.haskell.serde.nameToExpr(fn))(hydra.ext.haskell.serde.expressionToExpr(`val`))
  }
  val body: hydra.ast.Expr = hydra.serialization.commaSep(hydra.serialization.halfBlockStyle)(lists.map[hydra.ext.haskell.ast.FieldUpdate,
     hydra.ast.Expr](fromUpdate)(updates))
  hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.brackets(hydra.serialization.curlyBraces)(hydra.serialization.halfBlockStyle)(body))))
}

def fieldToExpr(field: hydra.ext.haskell.ast.Field): hydra.ast.Expr =
  {
  val name: hydra.ext.haskell.ast.Name = (field.name)
  val typ: hydra.ext.haskell.ast.Type = (field.`type`)
  hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.ext.haskell.serde.nameToExpr(name))(lists.cons[hydra.ast.Expr](hydra.serialization.cst("::"))(Seq(hydra.ext.haskell.serde.typeToExpr(typ)))))
}

def fieldWithCommentsToExpr(fieldWithComments: hydra.ext.haskell.ast.FieldWithComments): hydra.ast.Expr =
  {
  val field: hydra.ext.haskell.ast.Field = (fieldWithComments.field)
  val mc: Option[scala.Predef.String] = (fieldWithComments.comments)
  maybes.maybe[hydra.ast.Expr, scala.Predef.String](hydra.ext.haskell.serde.fieldToExpr(field))((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.ext.haskell.serde.toHaskellComments(c)))(Seq(hydra.ext.haskell.serde.fieldToExpr(field)))))(mc)
}

def ifExpressionToExpr(ifExpr: hydra.ext.haskell.ast.IfExpression): hydra.ast.Expr =
  {
  val eif: hydra.ext.haskell.ast.Expression = (ifExpr.condition)
  val ethen: hydra.ext.haskell.ast.Expression = (ifExpr.`then`)
  val eelse: hydra.ext.haskell.ast.Expression = (ifExpr.`else`)
  val ifOp: hydra.ast.Op = hydra.ast.Op("", hydra.ast.Padding(hydra.ast.Ws.none, hydra.ast.Ws.breakAndIndent("  ")), 0, hydra.ast.Associativity.none)
  val body: hydra.ast.Expr = hydra.serialization.newlineSep(lists.cons[hydra.ast.Expr](hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst("then"))(Seq(hydra.ext.haskell.serde.expressionToExpr(ethen)))))(Seq(hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst("else"))(Seq(hydra.ext.haskell.serde.expressionToExpr(eelse)))))))
  hydra.serialization.ifx(ifOp)(hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst("if"))(Seq(hydra.ext.haskell.serde.expressionToExpr(eif)))))(body)
}

def importExportSpecToExpr(spec: hydra.ext.haskell.ast.ImportExportSpec): hydra.ast.Expr = hydra.ext.haskell.serde.nameToExpr(spec.name)

def importToExpr(`import`: hydra.ext.haskell.ast.Import): hydra.ast.Expr =
  {
  val qual: Boolean = (`import`.qualified)
  val modName: hydra.ext.haskell.ast.ModuleName = (`import`.module)
  val mod: Option[hydra.ext.haskell.ast.ModuleName] = (`import`.as)
  val mspec: Option[hydra.ext.haskell.ast.SpecImport] = (`import`.spec)
  val name: scala.Predef.String = modName
  def hidingSec(spec: hydra.ext.haskell.ast.SpecImport): hydra.ast.Expr =
    spec match
    case hydra.ext.haskell.ast.SpecImport.hiding(v_SpecImport_hiding_names) => hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst("hiding "))(Seq(hydra.serialization.parens(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(lists.map[hydra.ext.haskell.ast.ImportExportSpec,
       hydra.ast.Expr](hydra.ext.haskell.serde.importExportSpecToExpr)(v_SpecImport_hiding_names))))))
  val parts: Seq[hydra.ast.Expr] = maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("import")),
     logic.ifElse[Option[hydra.ast.Expr]](qual)(Some(hydra.serialization.cst("qualified")))(None), Some(hydra.serialization.cst(name)),
     maybes.map[hydra.ext.haskell.ast.ModuleName, hydra.ast.Expr]((m: hydra.ext.haskell.ast.ModuleName) => hydra.serialization.cst(strings.cat2("as ")(m)))(mod),
     maybes.map[hydra.ext.haskell.ast.SpecImport, hydra.ast.Expr](hidingSec)(mspec)))
  hydra.serialization.spaceSep(parts)
}

def lambdaExpressionToExpr(lambdaExpr: hydra.ext.haskell.ast.LambdaExpression): hydra.ast.Expr =
  {
  val bindings: Seq[hydra.ext.haskell.ast.Pattern] = (lambdaExpr.bindings)
  val inner: hydra.ext.haskell.ast.Expression = (lambdaExpr.inner)
  val head: hydra.ast.Expr = hydra.serialization.spaceSep(lists.map[hydra.ext.haskell.ast.Pattern, hydra.ast.Expr](hydra.ext.haskell.serde.patternToExpr)(bindings))
  val body: hydra.ast.Expr = hydra.ext.haskell.serde.expressionToExpr(inner)
  hydra.serialization.ifx(hydra.ext.haskell.operators.lambdaOp)(hydra.serialization.prefix("\\")(head))(body)
}

def literalToExpr(lit: hydra.ext.haskell.ast.Literal): hydra.ast.Expr =
  {
  def parensIfNeg(b: Boolean)(e: scala.Predef.String): scala.Predef.String = logic.ifElse[scala.Predef.String](b)(strings.cat(Seq("(", e, ")")))(e)
  hydra.serialization.cst(lit match
    case hydra.ext.haskell.ast.Literal.char(v_Literal_char_c) => literals.showString(literals.showUint16(v_Literal_char_c))
    case hydra.ext.haskell.ast.Literal.double(v_Literal_double_d) => parensIfNeg(equality.lt[Double](v_Literal_double_d)(0.0))(literals.showFloat64(v_Literal_double_d))
    case hydra.ext.haskell.ast.Literal.float(v_Literal_float_f) => parensIfNeg(equality.lt[Float](v_Literal_float_f)(0.0f))(literals.showFloat32(v_Literal_float_f))
    case hydra.ext.haskell.ast.Literal.int(v_Literal_int_i) => parensIfNeg(equality.lt[Int](v_Literal_int_i)(0))(literals.showInt32(v_Literal_int_i))
    case hydra.ext.haskell.ast.Literal.integer(v_Literal_integer_i) => parensIfNeg(equality.lt[BigInt](v_Literal_integer_i)(BigInt(0L)))(literals.showBigint(v_Literal_integer_i))
    case hydra.ext.haskell.ast.Literal.string(v_Literal_string_s) => literals.showString(v_Literal_string_s))
}

def localBindingToExpr(binding: hydra.ext.haskell.ast.LocalBinding): hydra.ast.Expr =
  binding match
  case hydra.ext.haskell.ast.LocalBinding.signature(v_LocalBinding_signature_ts) => hydra.ext.haskell.serde.typeSignatureToExpr(v_LocalBinding_signature_ts)
  case hydra.ext.haskell.ast.LocalBinding.value(v_LocalBinding_value_vb) => hydra.ext.haskell.serde.valueBindingToExpr(v_LocalBinding_value_vb)

def moduleHeadToExpr(moduleHead: hydra.ext.haskell.ast.ModuleHead): hydra.ast.Expr =
  {
  val mc: Option[scala.Predef.String] = (moduleHead.comments)
  val modName: hydra.ext.haskell.ast.ModuleName = (moduleHead.name)
  val mname: scala.Predef.String = modName
  val head: hydra.ast.Expr = hydra.serialization.spaceSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst("module"))(lists.cons[hydra.ast.Expr](hydra.serialization.cst(mname))(Seq(hydra.serialization.cst("where")))))
  maybes.maybe[hydra.ast.Expr, scala.Predef.String](head)((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.ext.haskell.serde.toHaskellComments(c)))(lists.cons[hydra.ast.Expr](hydra.serialization.cst(""))(Seq(head)))))(mc)
}

def moduleToExpr(module: hydra.ext.haskell.ast.Module): hydra.ast.Expr =
  {
  val mh: Option[hydra.ext.haskell.ast.ModuleHead] = (module.head)
  val imports: Seq[hydra.ext.haskell.ast.Import] = (module.imports)
  val decls: Seq[hydra.ext.haskell.ast.DeclarationWithComments] = (module.declarations)
  val warning: Seq[hydra.ast.Expr] = Seq(hydra.serialization.cst(hydra.ext.haskell.serde.toSimpleComments(hydra.constants.warningAutoGeneratedFile)))
  val headerLine: Seq[hydra.ast.Expr] = maybes.maybe[Seq[hydra.ast.Expr], hydra.ext.haskell.ast.ModuleHead](Seq())((h: hydra.ext.haskell.ast.ModuleHead) => Seq(hydra.ext.haskell.serde.moduleHeadToExpr(h)))(mh)
  val declLines: Seq[hydra.ast.Expr] = lists.map[hydra.ext.haskell.ast.DeclarationWithComments, hydra.ast.Expr](hydra.ext.haskell.serde.declarationWithCommentsToExpr)(decls)
  val importLines: Seq[hydra.ast.Expr] = logic.ifElse[Seq[hydra.ast.Expr]](lists.`null`[hydra.ext.haskell.ast.Import](imports))(Seq())(Seq(hydra.serialization.newlineSep(lists.map[hydra.ext.haskell.ast.Import,
     hydra.ast.Expr](hydra.ext.haskell.serde.importToExpr)(imports))))
  hydra.serialization.doubleNewlineSep(lists.concat[hydra.ast.Expr](Seq(warning, headerLine, importLines, declLines)))
}

def nameToExpr(name: hydra.ext.haskell.ast.Name): hydra.ast.Expr =
  hydra.serialization.cst(name match
  case hydra.ext.haskell.ast.Name.`implicit`(v_Name_implicit_qn) => strings.cat2("?")(hydra.ext.haskell.serde.writeQualifiedName(v_Name_implicit_qn))
  case hydra.ext.haskell.ast.Name.normal(v_Name_normal_qn) => hydra.ext.haskell.serde.writeQualifiedName(v_Name_normal_qn)
  case hydra.ext.haskell.ast.Name.parens(v_Name_parens_qn) => strings.cat(Seq("(", hydra.ext.haskell.serde.writeQualifiedName(v_Name_parens_qn), ")")))

def patternToExpr(pat: hydra.ext.haskell.ast.Pattern): hydra.ast.Expr =
  pat match
  case hydra.ext.haskell.ast.Pattern.application(v_Pattern_application_app) => hydra.ext.haskell.serde.applicationPatternToExpr(v_Pattern_application_app)
  case hydra.ext.haskell.ast.Pattern.list(v_Pattern_list_pats) => hydra.serialization.bracketList(hydra.serialization.halfBlockStyle)(lists.map[hydra.ext.haskell.ast.Pattern,
     hydra.ast.Expr](hydra.ext.haskell.serde.patternToExpr)(v_Pattern_list_pats))
  case hydra.ext.haskell.ast.Pattern.literal(v_Pattern_literal_lit) => hydra.ext.haskell.serde.literalToExpr(v_Pattern_literal_lit)
  case hydra.ext.haskell.ast.Pattern.name(v_Pattern_name_name) => hydra.ext.haskell.serde.nameToExpr(v_Pattern_name_name)
  case hydra.ext.haskell.ast.Pattern.parens(v_Pattern_parens_pat_) => hydra.serialization.parenthesize(hydra.ext.haskell.serde.patternToExpr(`v_Pattern_parens_pat_`))
  case hydra.ext.haskell.ast.Pattern.tuple(v_Pattern_tuple_pats) => hydra.serialization.parenList(false)(lists.map[hydra.ext.haskell.ast.Pattern,
     hydra.ast.Expr](hydra.ext.haskell.serde.patternToExpr)(v_Pattern_tuple_pats))
  case hydra.ext.haskell.ast.Pattern.wildcard => hydra.serialization.cst("_")

def rightHandSideToExpr(rhs: hydra.ext.haskell.ast.RightHandSide): hydra.ast.Expr = hydra.ext.haskell.serde.expressionToExpr(rhs)

def statementToExpr(stmt: hydra.ext.haskell.ast.Statement): hydra.ast.Expr = hydra.ext.haskell.serde.expressionToExpr(stmt)

def typeSignatureToExpr(typeSig: hydra.ext.haskell.ast.TypeSignature): hydra.ast.Expr =
  {
  val name: hydra.ext.haskell.ast.Name = (typeSig.name)
  val typ: hydra.ext.haskell.ast.Type = (typeSig.`type`)
  val nameExpr: hydra.ast.Expr = hydra.ext.haskell.serde.nameToExpr(name)
  val typeExpr: hydra.ast.Expr = hydra.ext.haskell.serde.typeToExpr(typ)
  val inlineSig: hydra.ast.Expr = hydra.serialization.structuralSpaceSep(Seq(nameExpr, hydra.serialization.cst("::"), typeExpr))
  logic.ifElse[hydra.ast.Expr](equality.gt[Int](hydra.serialization.expressionLength(inlineSig))(120))(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(nameExpr,
     hydra.serialization.cst("::"))), hydra.serialization.tabIndent(typeExpr))))(inlineSig)
}

def typeToExpr(htype: hydra.ext.haskell.ast.Type): hydra.ast.Expr =
  htype match
  case hydra.ext.haskell.ast.Type.application(v_Type_application_appType) => {
    val lhs: hydra.ext.haskell.ast.Type = (v_Type_application_appType.context)
    val rhs: hydra.ext.haskell.ast.Type = (v_Type_application_appType.argument)
    hydra.serialization.ifx(hydra.ext.haskell.operators.appOp)(hydra.ext.haskell.serde.typeToExpr(lhs))(hydra.ext.haskell.serde.typeToExpr(rhs))
  }
  case hydra.ext.haskell.ast.Type.ctx(v_Type_ctx_ctxType) => {
    val ctx: hydra.ext.haskell.ast.Assertion = (v_Type_ctx_ctxType.ctx)
    val typ: hydra.ext.haskell.ast.Type = (v_Type_ctx_ctxType.`type`)
    hydra.serialization.ifx(hydra.ext.haskell.operators.assertOp)(hydra.ext.haskell.serde.assertionToExpr(ctx))(hydra.ext.haskell.serde.typeToExpr(typ))
  }
  case hydra.ext.haskell.ast.Type.function(v_Type_function_funType) => {
    val dom: hydra.ext.haskell.ast.Type = (v_Type_function_funType.domain)
    val cod: hydra.ext.haskell.ast.Type = (v_Type_function_funType.codomain)
    hydra.serialization.ifx(hydra.ext.haskell.operators.arrowOp)(hydra.ext.haskell.serde.typeToExpr(dom))(hydra.ext.haskell.serde.typeToExpr(cod))
  }
  case hydra.ext.haskell.ast.Type.list(v_Type_list_htype_) => hydra.serialization.bracketList(hydra.serialization.inlineStyle)(Seq(hydra.ext.haskell.serde.typeToExpr(`v_Type_list_htype_`)))
  case hydra.ext.haskell.ast.Type.tuple(v_Type_tuple_types) => hydra.serialization.parenList(false)(lists.map[hydra.ext.haskell.ast.Type,
     hydra.ast.Expr](hydra.ext.haskell.serde.typeToExpr)(v_Type_tuple_types))
  case hydra.ext.haskell.ast.Type.variable(v_Type_variable_name) => hydra.ext.haskell.serde.nameToExpr(v_Type_variable_name)

def valueBindingToExpr(vb: hydra.ext.haskell.ast.ValueBinding): hydra.ast.Expr =
  vb match
  case hydra.ext.haskell.ast.ValueBinding.simple(v_ValueBinding_simple_simpleVB) => {
    val pat: hydra.ext.haskell.ast.Pattern = (v_ValueBinding_simple_simpleVB.pattern)
    val rhs: hydra.ext.haskell.ast.RightHandSide = (v_ValueBinding_simple_simpleVB.rhs)
    val local: Option[hydra.ext.haskell.ast.LocalBindings] = (v_ValueBinding_simple_simpleVB.localBindings)
    val lhsExpr: hydra.ast.Expr = hydra.ext.haskell.serde.patternToExpr(pat)
    val rhsExpr: hydra.ast.Expr = hydra.ext.haskell.serde.rightHandSideToExpr(rhs)
    val inlineBody: hydra.ast.Expr = hydra.serialization.structuralSpaceSep(Seq(lhsExpr, hydra.serialization.cst("="), rhsExpr))
    val body: hydra.ast.Expr = logic.ifElse[hydra.ast.Expr](equality.gt[Int](hydra.serialization.expressionLength(inlineBody))(120))(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(lhsExpr,
       hydra.serialization.cst("="))), hydra.serialization.tabIndent(rhsExpr))))(inlineBody)
    maybes.maybe[hydra.ast.Expr, hydra.ext.haskell.ast.LocalBindings](body)((localBindings: hydra.ext.haskell.ast.LocalBindings) =>
      {
      val bindings: Seq[hydra.ext.haskell.ast.LocalBinding] = localBindings
      hydra.serialization.indentBlock(lists.cons[hydra.ast.Expr](body)(Seq(hydra.serialization.indentBlock(lists.cons[hydra.ast.Expr](hydra.serialization.cst("where"))(lists.map[hydra.ext.haskell.ast.LocalBinding,
         hydra.ast.Expr](hydra.ext.haskell.serde.localBindingToExpr)(bindings))))))
    })(local)
  }

def variableToExpr(variable: hydra.ext.haskell.ast.Variable): hydra.ast.Expr = hydra.ext.haskell.serde.nameToExpr(variable)

def toHaskellComments(c: scala.Predef.String): scala.Predef.String =
  strings.intercalate("\n")(lists.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => strings.cat2("-- | ")(s))(strings.lines(c)))

def toSimpleComments(c: scala.Predef.String): scala.Predef.String =
  strings.intercalate("\n")(lists.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => strings.cat2("-- ")(s))(strings.lines(c)))

def writeQualifiedName(qname: hydra.ext.haskell.ast.QualifiedName): scala.Predef.String =
  {
  val qualifiers: Seq[hydra.ext.haskell.ast.NamePart] = (qname.qualifiers)
  val unqual: hydra.ext.haskell.ast.NamePart = (qname.unqualified)
  def h(namePart: hydra.ext.haskell.ast.NamePart): scala.Predef.String = namePart
  val allParts: Seq[scala.Predef.String] = lists.concat2[scala.Predef.String](lists.map[hydra.ext.haskell.ast.NamePart,
     scala.Predef.String](h)(qualifiers))(Seq(h(unqual)))
  strings.intercalate(".")(allParts)
}
