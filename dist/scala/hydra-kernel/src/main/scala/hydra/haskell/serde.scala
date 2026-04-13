package hydra.haskell.serde

import hydra.ast.*

import hydra.haskell.syntax.*

def alternativeToExpr(alt: hydra.haskell.syntax.Alternative): hydra.ast.Expr =
  hydra.serialization.structuralSpaceSep(Seq(hydra.haskell.serde.patternToExpr(alt.pattern), hydra.serialization.cst("->"),
     hydra.haskell.serde.caseRhsToExpr(alt.rhs)))

def applicationExpressionToExpr(app: hydra.haskell.syntax.ApplicationExpression): hydra.ast.Expr =
  hydra.serialization.ifx(hydra.haskell.operators.appOp)(hydra.haskell.serde.expressionToExpr(app.function))(hydra.haskell.serde.expressionToExpr(app.argument))

def applicationPatternToExpr(appPat: hydra.haskell.syntax.ApplicationPattern): hydra.ast.Expr =
  {
  lazy val name: hydra.haskell.syntax.Name = (appPat.name)
  lazy val pats: Seq[hydra.haskell.syntax.Pattern] = (appPat.args)
  hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.nameToExpr(name))(hydra.lib.lists.map[hydra.haskell.syntax.Pattern,
     hydra.ast.Expr](hydra.haskell.serde.patternToExpr)(pats)))
}

def assertionToExpr(sert: hydra.haskell.syntax.Assertion): hydra.ast.Expr =
  sert match
  case hydra.haskell.syntax.Assertion.`class`(v_Assertion_class_cls) => hydra.haskell.serde.classAssertionToExpr(v_Assertion_class_cls)
  case hydra.haskell.syntax.Assertion.tuple(v_Assertion_tuple_serts) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.haskell.syntax.Assertion,
     hydra.ast.Expr](hydra.haskell.serde.assertionToExpr)(v_Assertion_tuple_serts))

def caseExpressionToExpr(caseExpr: hydra.haskell.syntax.CaseExpression): hydra.ast.Expr =
  {
  lazy val cs: hydra.haskell.syntax.Expression = (caseExpr.`case`)
  lazy val alts: Seq[hydra.haskell.syntax.Alternative] = (caseExpr.alternatives)
  lazy val ofOp: hydra.ast.Op = hydra.ast.Op("of", hydra.ast.Padding(hydra.ast.Ws.space, hydra.ast.Ws.breakAndIndent("  ")), 0, hydra.ast.Associativity.none)
  lazy val lhs: hydra.ast.Expr = hydra.serialization.spaceSep(Seq(hydra.serialization.cst("case"), hydra.haskell.serde.expressionToExpr(cs)))
  lazy val rhs: hydra.ast.Expr = hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.haskell.syntax.Alternative,
     hydra.ast.Expr](hydra.haskell.serde.alternativeToExpr)(alts))
  hydra.serialization.ifx(ofOp)(lhs)(rhs)
}

def caseRhsToExpr(rhs: hydra.haskell.syntax.CaseRhs): hydra.ast.Expr = hydra.haskell.serde.expressionToExpr(rhs)

def classAssertionToExpr(clsAsrt: hydra.haskell.syntax.ClassAssertion): hydra.ast.Expr =
  {
  lazy val name: hydra.haskell.syntax.Name = (clsAsrt.name)
  lazy val types: Seq[hydra.haskell.syntax.Type] = (clsAsrt.types)
  hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.commaSep(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.haskell.syntax.Type,
     hydra.ast.Expr](hydra.haskell.serde.typeToExpr)(types)))))
}

def constructRecordExpressionToExpr(constructRecord: hydra.haskell.syntax.ConstructRecordExpression): hydra.ast.Expr =
  {
  lazy val name: hydra.haskell.syntax.Name = (constructRecord.name)
  lazy val updates: Seq[hydra.haskell.syntax.FieldUpdate] = (constructRecord.fields)
  def fromUpdate(update: hydra.haskell.syntax.FieldUpdate): hydra.ast.Expr =
    {
    lazy val fn: hydra.haskell.syntax.Name = (update.name)
    lazy val `val`: hydra.haskell.syntax.Expression = (update.value)
    hydra.serialization.ifx(hydra.haskell.operators.defineOp)(hydra.haskell.serde.nameToExpr(fn))(hydra.haskell.serde.expressionToExpr(`val`))
  }
  lazy val body: hydra.ast.Expr = hydra.serialization.commaSep(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.haskell.syntax.FieldUpdate,
     hydra.ast.Expr](fromUpdate)(updates))
  hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.brackets(hydra.serialization.curlyBraces)(hydra.serialization.halfBlockStyle)(body))))
}

def constructorToExpr(cons: hydra.haskell.syntax.Constructor): hydra.ast.Expr =
  cons match
  case hydra.haskell.syntax.Constructor.ordinary(v_Constructor_ordinary_ord) => {
    lazy val name: hydra.haskell.syntax.Name = (v_Constructor_ordinary_ord.name)
    lazy val types: Seq[hydra.haskell.syntax.Type] = (v_Constructor_ordinary_ord.fields)
    hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.haskell.syntax.Type,
       hydra.ast.Expr](hydra.haskell.serde.typeToExpr)(types)))))
  }
  case hydra.haskell.syntax.Constructor.record(v_Constructor_record_rec) => {
    lazy val name: hydra.haskell.syntax.Name = (v_Constructor_record_rec.name)
    lazy val fields: Seq[hydra.haskell.syntax.FieldWithComments] = (v_Constructor_record_rec.fields)
    hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.nameToExpr(name))(Seq(hydra.serialization.curlyBracesList(None)(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.haskell.syntax.FieldWithComments,
       hydra.ast.Expr](hydra.haskell.serde.fieldWithCommentsToExpr)(fields)))))
  }

def constructorWithCommentsToExpr(consWithComments: hydra.haskell.syntax.ConstructorWithComments): hydra.ast.Expr =
  {
  lazy val body: hydra.haskell.syntax.Constructor = (consWithComments.body)
  lazy val mc: Option[scala.Predef.String] = (consWithComments.comments)
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](hydra.haskell.serde.constructorToExpr(body))((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.haskell.serde.toHaskellComments(c)))(Seq(hydra.haskell.serde.constructorToExpr(body)))))(mc)
}

def dataOrNewtypeToExpr(kw: hydra.haskell.syntax.DataOrNewtype): hydra.ast.Expr =
  kw match
  case hydra.haskell.syntax.DataOrNewtype.data => hydra.serialization.cst("data")
  case hydra.haskell.syntax.DataOrNewtype.newtype => hydra.serialization.cst("newtype")

def declarationHeadToExpr(hd: hydra.haskell.syntax.DeclarationHead): hydra.ast.Expr =
  hd match
  case hydra.haskell.syntax.DeclarationHead.application(v_DeclarationHead_application_appHead) => {
    lazy val fun: hydra.haskell.syntax.DeclarationHead = (v_DeclarationHead_application_appHead.function)
    lazy val op: hydra.haskell.syntax.Variable = (v_DeclarationHead_application_appHead.operand)
    hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.declarationHeadToExpr(fun))(Seq(hydra.haskell.serde.variableToExpr(op))))
  }
  case hydra.haskell.syntax.DeclarationHead.simple(v_DeclarationHead_simple_name) => hydra.haskell.serde.nameToExpr(v_DeclarationHead_simple_name)

def declarationToExpr(decl: hydra.haskell.syntax.Declaration): hydra.ast.Expr =
  decl match
  case hydra.haskell.syntax.Declaration.data(v_Declaration_data_dataDecl) => {
    lazy val kw: hydra.haskell.syntax.DataOrNewtype = (v_Declaration_data_dataDecl.keyword)
    lazy val hd: hydra.haskell.syntax.DeclarationHead = (v_Declaration_data_dataDecl.head)
    lazy val cons: Seq[hydra.haskell.syntax.ConstructorWithComments] = (v_Declaration_data_dataDecl.constructors)
    lazy val deriv: Seq[hydra.haskell.syntax.Deriving] = (v_Declaration_data_dataDecl.deriving)
    lazy val derivCat: Seq[hydra.haskell.syntax.Name] = hydra.lib.lists.concat[hydra.haskell.syntax.Name](hydra.lib.lists.map[hydra.haskell.syntax.Deriving,
       Seq[hydra.haskell.syntax.Name]]((x) => x)(deriv))
    lazy val constructors: hydra.ast.Expr = hydra.serialization.orSep(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.haskell.syntax.ConstructorWithComments,
       hydra.ast.Expr](hydra.haskell.serde.constructorWithCommentsToExpr)(cons))
    lazy val derivingClause: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.haskell.syntax.Name](derivCat))(Seq())(Seq(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("deriving"))(Seq(hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.haskell.syntax.Name,
       hydra.ast.Expr](hydra.haskell.serde.nameToExpr)(derivCat)))))))
    lazy val mainParts: Seq[hydra.ast.Expr] = Seq(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.dataOrNewtypeToExpr(kw))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.declarationHeadToExpr(hd))(Seq(hydra.serialization.cst("="))))),
       constructors)
    hydra.serialization.indentBlock(hydra.lib.lists.concat2[hydra.ast.Expr](mainParts)(derivingClause))
  }
  case hydra.haskell.syntax.Declaration.`type`(v_Declaration_type_typeDecl) => {
    lazy val hd: hydra.haskell.syntax.DeclarationHead = (v_Declaration_type_typeDecl.name)
    lazy val typ: hydra.haskell.syntax.Type = (v_Declaration_type_typeDecl.`type`)
    hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("type"))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.declarationHeadToExpr(hd))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("="))(Seq(hydra.haskell.serde.typeToExpr(typ))))))
  }
  case hydra.haskell.syntax.Declaration.valueBinding(v_Declaration_valueBinding_vb) => hydra.haskell.serde.valueBindingToExpr(v_Declaration_valueBinding_vb)
  case hydra.haskell.syntax.Declaration.typedBinding(v_Declaration_typedBinding_typedBinding) => {
    lazy val typeSig: hydra.haskell.syntax.TypeSignature = (v_Declaration_typedBinding_typedBinding.typeSignature)
    lazy val vb: hydra.haskell.syntax.ValueBinding = (v_Declaration_typedBinding_typedBinding.valueBinding)
    lazy val name: hydra.haskell.syntax.Name = (typeSig.name)
    lazy val htype: hydra.haskell.syntax.Type = (typeSig.`type`)
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.structuralSpaceSep(Seq(hydra.haskell.serde.nameToExpr(name),
       hydra.serialization.cst("::"), hydra.haskell.serde.typeToExpr(htype))))(Seq(hydra.haskell.serde.valueBindingToExpr(vb))))
  }

def declarationWithCommentsToExpr(declWithComments: hydra.haskell.syntax.DeclarationWithComments): hydra.ast.Expr =
  {
  lazy val body: hydra.haskell.syntax.Declaration = (declWithComments.body)
  lazy val mc: Option[scala.Predef.String] = (declWithComments.comments)
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](hydra.haskell.serde.declarationToExpr(body))((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.haskell.serde.toHaskellComments(c)))(Seq(hydra.haskell.serde.declarationToExpr(body)))))(mc)
}

def expressionToExpr(expr: hydra.haskell.syntax.Expression): hydra.ast.Expr =
  expr match
  case hydra.haskell.syntax.Expression.application(v_Expression_application_app) => hydra.haskell.serde.applicationExpressionToExpr(v_Expression_application_app)
  case hydra.haskell.syntax.Expression.`case`(v_Expression_case_cases) => hydra.haskell.serde.caseExpressionToExpr(v_Expression_case_cases)
  case hydra.haskell.syntax.Expression.constructRecord(v_Expression_constructRecord_r) => hydra.haskell.serde.constructRecordExpressionToExpr(v_Expression_constructRecord_r)
  case hydra.haskell.syntax.Expression.`do`(v_Expression_do_statements) => hydra.serialization.indentBlock(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("do"))(hydra.lib.lists.map[hydra.haskell.syntax.Statement,
     hydra.ast.Expr](hydra.haskell.serde.statementToExpr)(v_Expression_do_statements)))
  case hydra.haskell.syntax.Expression.`if`(v_Expression_if_ifte) => hydra.haskell.serde.ifExpressionToExpr(v_Expression_if_ifte)
  case hydra.haskell.syntax.Expression.literal(v_Expression_literal_lit) => hydra.haskell.serde.literalToExpr(v_Expression_literal_lit)
  case hydra.haskell.syntax.Expression.lambda(v_Expression_lambda_lam) => hydra.serialization.parenthesize(hydra.haskell.serde.lambdaExpressionToExpr(v_Expression_lambda_lam))
  case hydra.haskell.syntax.Expression.let(v_Expression_let_letExpr) => {
    lazy val bindings: Seq[hydra.haskell.syntax.LocalBinding] = (v_Expression_let_letExpr.bindings)
    lazy val inner: hydra.haskell.syntax.Expression = (v_Expression_let_letExpr.inner)
    def encodeBinding(binding: hydra.haskell.syntax.LocalBinding): hydra.ast.Expr =
      hydra.serialization.indentSubsequentLines("    ")(hydra.haskell.serde.localBindingToExpr(binding))
    hydra.serialization.indentBlock(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(""))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("let"))(Seq(hydra.serialization.customIndentBlock("    ")(hydra.lib.lists.map[hydra.haskell.syntax.LocalBinding,
       hydra.ast.Expr](encodeBinding)(bindings))))))(Seq(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("in"))(Seq(hydra.haskell.serde.expressionToExpr(inner))))))))
  }
  case hydra.haskell.syntax.Expression.list(v_Expression_list_exprs) => hydra.serialization.bracketList(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.haskell.syntax.Expression,
     hydra.ast.Expr](hydra.haskell.serde.expressionToExpr)(v_Expression_list_exprs))
  case hydra.haskell.syntax.Expression.parens(v_Expression_parens_expr_) => hydra.serialization.parenthesize(hydra.haskell.serde.expressionToExpr(`v_Expression_parens_expr_`))
  case hydra.haskell.syntax.Expression.tuple(v_Expression_tuple_exprs) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.haskell.syntax.Expression,
     hydra.ast.Expr](hydra.haskell.serde.expressionToExpr)(v_Expression_tuple_exprs))
  case hydra.haskell.syntax.Expression.variable(v_Expression_variable_name) => hydra.haskell.serde.nameToExpr(v_Expression_variable_name)

def fieldToExpr(field: hydra.haskell.syntax.Field): hydra.ast.Expr =
  {
  lazy val name: hydra.haskell.syntax.Name = (field.name)
  lazy val typ: hydra.haskell.syntax.Type = (field.`type`)
  hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.haskell.serde.nameToExpr(name))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("::"))(Seq(hydra.haskell.serde.typeToExpr(typ)))))
}

def fieldWithCommentsToExpr(fieldWithComments: hydra.haskell.syntax.FieldWithComments): hydra.ast.Expr =
  {
  lazy val field: hydra.haskell.syntax.Field = (fieldWithComments.field)
  lazy val mc: Option[scala.Predef.String] = (fieldWithComments.comments)
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](hydra.haskell.serde.fieldToExpr(field))((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.haskell.serde.toHaskellComments(c)))(Seq(hydra.haskell.serde.fieldToExpr(field)))))(mc)
}

def ifExpressionToExpr(ifExpr: hydra.haskell.syntax.IfExpression): hydra.ast.Expr =
  {
  lazy val eif: hydra.haskell.syntax.Expression = (ifExpr.condition)
  lazy val ethen: hydra.haskell.syntax.Expression = (ifExpr.`then`)
  lazy val eelse: hydra.haskell.syntax.Expression = (ifExpr.`else`)
  lazy val ifOp: hydra.ast.Op = hydra.ast.Op("", hydra.ast.Padding(hydra.ast.Ws.none, hydra.ast.Ws.breakAndIndent("  ")), 0, hydra.ast.Associativity.none)
  lazy val body: hydra.ast.Expr = hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("then"))(Seq(hydra.haskell.serde.expressionToExpr(ethen)))))(Seq(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("else"))(Seq(hydra.haskell.serde.expressionToExpr(eelse)))))))
  hydra.serialization.ifx(ifOp)(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("if"))(Seq(hydra.haskell.serde.expressionToExpr(eif)))))(body)
}

def importExportSpecToExpr(spec: hydra.haskell.syntax.ImportExportSpec): hydra.ast.Expr = hydra.haskell.serde.nameToExpr(spec.name)

def importToExpr(`import`: hydra.haskell.syntax.Import): hydra.ast.Expr =
  {
  lazy val qual: Boolean = (`import`.qualified)
  lazy val modName: hydra.haskell.syntax.ModuleName = (`import`.module)
  lazy val mod: Option[hydra.haskell.syntax.ModuleName] = (`import`.as)
  lazy val mspec: Option[hydra.haskell.syntax.SpecImport] = (`import`.spec)
  lazy val name: scala.Predef.String = modName
  def hidingSec(spec: hydra.haskell.syntax.SpecImport): hydra.ast.Expr =
    spec match
    case hydra.haskell.syntax.SpecImport.hiding(v_SpecImport_hiding_names) => hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("hiding "))(Seq(hydra.serialization.parens(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.haskell.syntax.ImportExportSpec,
       hydra.ast.Expr](hydra.haskell.serde.importExportSpecToExpr)(v_SpecImport_hiding_names))))))
  lazy val parts: Seq[hydra.ast.Expr] = hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("import")),
     hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](qual)(Some(hydra.serialization.cst("qualified")))(None),
     Some(hydra.serialization.cst(name)), hydra.lib.maybes.map[hydra.haskell.syntax.ModuleName, hydra.ast.Expr]((m: hydra.haskell.syntax.ModuleName) => hydra.serialization.cst(hydra.lib.strings.cat2("as ")(m)))(mod),
     hydra.lib.maybes.map[hydra.haskell.syntax.SpecImport, hydra.ast.Expr](hidingSec)(mspec)))
  hydra.serialization.spaceSep(parts)
}

def lambdaExpressionToExpr(lambdaExpr: hydra.haskell.syntax.LambdaExpression): hydra.ast.Expr =
  {
  lazy val bindings: Seq[hydra.haskell.syntax.Pattern] = (lambdaExpr.bindings)
  lazy val inner: hydra.haskell.syntax.Expression = (lambdaExpr.inner)
  lazy val head: hydra.ast.Expr = hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.haskell.syntax.Pattern,
     hydra.ast.Expr](hydra.haskell.serde.patternToExpr)(bindings))
  lazy val body: hydra.ast.Expr = hydra.haskell.serde.expressionToExpr(inner)
  hydra.serialization.ifx(hydra.haskell.operators.lambdaOp)(hydra.serialization.prefix("\\")(head))(body)
}

def literalToExpr(lit: hydra.haskell.syntax.Literal): hydra.ast.Expr =
  {
  def parensIfNeg(b: Boolean)(e: scala.Predef.String): scala.Predef.String =
    hydra.lib.logic.ifElse[scala.Predef.String](b)(hydra.lib.strings.cat(Seq("(", e, ")")))(e)
  def showFloat[T0](showFn: (T0 => scala.Predef.String))(v: T0): scala.Predef.String =
    {
    lazy val raw: scala.Predef.String = showFn(v)
    hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](raw)("NaN"))("(0/0)")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](raw)("Infinity"))("(1/0)")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](raw)("-Infinity"))("(-(1/0))")(parensIfNeg(hydra.lib.equality.equal[Int](hydra.lib.strings.charAt(0)(raw))(45))(raw))))
  }
  hydra.serialization.cst(lit match
    case hydra.haskell.syntax.Literal.char(v_Literal_char_c) => hydra.lib.literals.showString(hydra.lib.literals.showUint16(v_Literal_char_c))
    case hydra.haskell.syntax.Literal.double(v_Literal_double_d) => showFloat((v: Double) => hydra.lib.literals.showFloat64(v))(v_Literal_double_d)
    case hydra.haskell.syntax.Literal.float(v_Literal_float_f) => showFloat((v: Float) => hydra.lib.literals.showFloat32(v))(v_Literal_float_f)
    case hydra.haskell.syntax.Literal.int(v_Literal_int_i) => parensIfNeg(hydra.lib.equality.lt[Int](v_Literal_int_i)(0))(hydra.lib.literals.showInt32(v_Literal_int_i))
    case hydra.haskell.syntax.Literal.integer(v_Literal_integer_i) => parensIfNeg(hydra.lib.equality.lt[BigInt](v_Literal_integer_i)(BigInt("0")))(hydra.lib.literals.showBigint(v_Literal_integer_i))
    case hydra.haskell.syntax.Literal.string(v_Literal_string_s) => hydra.lib.literals.showString(v_Literal_string_s))
}

def localBindingToExpr(binding: hydra.haskell.syntax.LocalBinding): hydra.ast.Expr =
  binding match
  case hydra.haskell.syntax.LocalBinding.signature(v_LocalBinding_signature_ts) => hydra.haskell.serde.typeSignatureToExpr(v_LocalBinding_signature_ts)
  case hydra.haskell.syntax.LocalBinding.value(v_LocalBinding_value_vb) => hydra.haskell.serde.valueBindingToExpr(v_LocalBinding_value_vb)

def moduleHeadToExpr(moduleHead: hydra.haskell.syntax.ModuleHead): hydra.ast.Expr =
  {
  lazy val mc: Option[scala.Predef.String] = (moduleHead.comments)
  lazy val modName: hydra.haskell.syntax.ModuleName = (moduleHead.name)
  lazy val mname: scala.Predef.String = modName
  lazy val head: hydra.ast.Expr = hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("module"))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(mname))(Seq(hydra.serialization.cst("where")))))
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](head)((c: scala.Predef.String) =>
    hydra.serialization.newlineSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(hydra.haskell.serde.toHaskellComments(c)))(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst(""))(Seq(head)))))(mc)
}

def moduleToExpr(module: hydra.haskell.syntax.Module): hydra.ast.Expr =
  {
  lazy val mh: Option[hydra.haskell.syntax.ModuleHead] = (module.head)
  lazy val imports: Seq[hydra.haskell.syntax.Import] = (module.imports)
  lazy val decls: Seq[hydra.haskell.syntax.DeclarationWithComments] = (module.declarations)
  lazy val warning: Seq[hydra.ast.Expr] = Seq(hydra.serialization.cst(hydra.haskell.serde.toSimpleComments(hydra.constants.warningAutoGeneratedFile)))
  lazy val headerLine: Seq[hydra.ast.Expr] = hydra.lib.maybes.maybe[Seq[hydra.ast.Expr], hydra.haskell.syntax.ModuleHead](Seq())((h: hydra.haskell.syntax.ModuleHead) => Seq(hydra.haskell.serde.moduleHeadToExpr(h)))(mh)
  lazy val declLines: Seq[hydra.ast.Expr] = hydra.lib.lists.map[hydra.haskell.syntax.DeclarationWithComments,
     hydra.ast.Expr](hydra.haskell.serde.declarationWithCommentsToExpr)(decls)
  lazy val importLines: Seq[hydra.ast.Expr] = hydra.lib.logic.ifElse[Seq[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.haskell.syntax.Import](imports))(Seq())(Seq(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.haskell.syntax.Import,
     hydra.ast.Expr](hydra.haskell.serde.importToExpr)(imports))))
  hydra.serialization.doubleNewlineSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(warning, headerLine, importLines, declLines)))
}

def nameToExpr(name: hydra.haskell.syntax.Name): hydra.ast.Expr =
  hydra.serialization.cst(name match
  case hydra.haskell.syntax.Name.`implicit`(v_Name_implicit_qn) => hydra.lib.strings.cat2("?")(hydra.haskell.serde.writeQualifiedName(v_Name_implicit_qn))
  case hydra.haskell.syntax.Name.normal(v_Name_normal_qn) => hydra.haskell.serde.writeQualifiedName(v_Name_normal_qn)
  case hydra.haskell.syntax.Name.parens(v_Name_parens_qn) => hydra.lib.strings.cat(Seq("(", hydra.haskell.serde.writeQualifiedName(v_Name_parens_qn), ")")))

def patternToExpr(pat: hydra.haskell.syntax.Pattern): hydra.ast.Expr =
  pat match
  case hydra.haskell.syntax.Pattern.application(v_Pattern_application_app) => hydra.haskell.serde.applicationPatternToExpr(v_Pattern_application_app)
  case hydra.haskell.syntax.Pattern.list(v_Pattern_list_pats) => hydra.serialization.bracketList(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.haskell.syntax.Pattern,
     hydra.ast.Expr](hydra.haskell.serde.patternToExpr)(v_Pattern_list_pats))
  case hydra.haskell.syntax.Pattern.literal(v_Pattern_literal_lit) => hydra.haskell.serde.literalToExpr(v_Pattern_literal_lit)
  case hydra.haskell.syntax.Pattern.name(v_Pattern_name_name) => hydra.haskell.serde.nameToExpr(v_Pattern_name_name)
  case hydra.haskell.syntax.Pattern.parens(v_Pattern_parens_pat_) => hydra.serialization.parenthesize(hydra.haskell.serde.patternToExpr(`v_Pattern_parens_pat_`))
  case hydra.haskell.syntax.Pattern.tuple(v_Pattern_tuple_pats) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.haskell.syntax.Pattern,
     hydra.ast.Expr](hydra.haskell.serde.patternToExpr)(v_Pattern_tuple_pats))
  case hydra.haskell.syntax.Pattern.wildcard => hydra.serialization.cst("_")

def rightHandSideToExpr(rhs: hydra.haskell.syntax.RightHandSide): hydra.ast.Expr = hydra.haskell.serde.expressionToExpr(rhs)

def statementToExpr(stmt: hydra.haskell.syntax.Statement): hydra.ast.Expr = hydra.haskell.serde.expressionToExpr(stmt)

def toHaskellComments(c: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.intercalate("\n")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => hydra.lib.strings.cat2("-- | ")(s))(hydra.lib.strings.lines(c)))

def toSimpleComments(c: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.intercalate("\n")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => hydra.lib.strings.cat2("-- ")(s))(hydra.lib.strings.lines(c)))

def typeSignatureToExpr(typeSig: hydra.haskell.syntax.TypeSignature): hydra.ast.Expr =
  {
  lazy val name: hydra.haskell.syntax.Name = (typeSig.name)
  lazy val typ: hydra.haskell.syntax.Type = (typeSig.`type`)
  lazy val nameExpr: hydra.ast.Expr = hydra.haskell.serde.nameToExpr(name)
  lazy val typeExpr: hydra.ast.Expr = hydra.haskell.serde.typeToExpr(typ)
  lazy val inlineSig: hydra.ast.Expr = hydra.serialization.structuralSpaceSep(Seq(nameExpr, hydra.serialization.cst("::"), typeExpr))
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.gt[Int](hydra.serialization.expressionLength(inlineSig))(120))(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(nameExpr,
     hydra.serialization.cst("::"))), hydra.serialization.tabIndent(typeExpr))))(inlineSig)
}

def typeToExpr(htype: hydra.haskell.syntax.Type): hydra.ast.Expr =
  htype match
  case hydra.haskell.syntax.Type.application(v_Type_application_appType) => {
    lazy val lhs: hydra.haskell.syntax.Type = (v_Type_application_appType.context)
    lazy val rhs: hydra.haskell.syntax.Type = (v_Type_application_appType.argument)
    hydra.serialization.ifx(hydra.haskell.operators.appOp)(hydra.haskell.serde.typeToExpr(lhs))(hydra.haskell.serde.typeToExpr(rhs))
  }
  case hydra.haskell.syntax.Type.ctx(v_Type_ctx_ctxType) => {
    lazy val ctx: hydra.haskell.syntax.Assertion = (v_Type_ctx_ctxType.ctx)
    lazy val typ: hydra.haskell.syntax.Type = (v_Type_ctx_ctxType.`type`)
    hydra.serialization.ifx(hydra.haskell.operators.assertOp)(hydra.haskell.serde.assertionToExpr(ctx))(hydra.haskell.serde.typeToExpr(typ))
  }
  case hydra.haskell.syntax.Type.function(v_Type_function_funType) => {
    lazy val dom: hydra.haskell.syntax.Type = (v_Type_function_funType.domain)
    lazy val cod: hydra.haskell.syntax.Type = (v_Type_function_funType.codomain)
    hydra.serialization.ifx(hydra.haskell.operators.arrowOp)(hydra.haskell.serde.typeToExpr(dom))(hydra.haskell.serde.typeToExpr(cod))
  }
  case hydra.haskell.syntax.Type.list(v_Type_list_htype_) => hydra.serialization.bracketList(hydra.serialization.inlineStyle)(Seq(hydra.haskell.serde.typeToExpr(`v_Type_list_htype_`)))
  case hydra.haskell.syntax.Type.tuple(v_Type_tuple_types) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.haskell.syntax.Type,
     hydra.ast.Expr](hydra.haskell.serde.typeToExpr)(v_Type_tuple_types))
  case hydra.haskell.syntax.Type.variable(v_Type_variable_name) => hydra.haskell.serde.nameToExpr(v_Type_variable_name)

def valueBindingToExpr(vb: hydra.haskell.syntax.ValueBinding): hydra.ast.Expr =
  vb match
  case hydra.haskell.syntax.ValueBinding.simple(v_ValueBinding_simple_simpleVB) => {
    lazy val pat: hydra.haskell.syntax.Pattern = (v_ValueBinding_simple_simpleVB.pattern)
    lazy val rhs: hydra.haskell.syntax.RightHandSide = (v_ValueBinding_simple_simpleVB.rhs)
    lazy val local: Option[hydra.haskell.syntax.LocalBindings] = (v_ValueBinding_simple_simpleVB.localBindings)
    lazy val lhsExpr: hydra.ast.Expr = hydra.haskell.serde.patternToExpr(pat)
    lazy val rhsExpr: hydra.ast.Expr = hydra.haskell.serde.rightHandSideToExpr(rhs)
    lazy val inlineBody: hydra.ast.Expr = hydra.serialization.structuralSpaceSep(Seq(lhsExpr, hydra.serialization.cst("="), rhsExpr))
    lazy val body: hydra.ast.Expr = hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.gt[Int](hydra.serialization.expressionLength(inlineBody))(120))(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(lhsExpr,
       hydra.serialization.cst("="))), hydra.serialization.tabIndent(rhsExpr))))(inlineBody)
    hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.haskell.syntax.LocalBindings](body)((localBindings: hydra.haskell.syntax.LocalBindings) =>
      {
      lazy val bindings: Seq[hydra.haskell.syntax.LocalBinding] = localBindings
      hydra.serialization.indentBlock(hydra.lib.lists.cons[hydra.ast.Expr](body)(Seq(hydra.serialization.indentBlock(hydra.lib.lists.cons[hydra.ast.Expr](hydra.serialization.cst("where"))(hydra.lib.lists.map[hydra.haskell.syntax.LocalBinding,
         hydra.ast.Expr](hydra.haskell.serde.localBindingToExpr)(bindings))))))
    })(local)
  }

def variableToExpr(variable: hydra.haskell.syntax.Variable): hydra.ast.Expr = hydra.haskell.serde.nameToExpr(variable)

def writeQualifiedName(qname: hydra.haskell.syntax.QualifiedName): scala.Predef.String =
  {
  lazy val qualifiers: Seq[hydra.haskell.syntax.NamePart] = (qname.qualifiers)
  lazy val unqual: hydra.haskell.syntax.NamePart = (qname.unqualified)
  def h(namePart: hydra.haskell.syntax.NamePart): scala.Predef.String = namePart
  lazy val allParts: Seq[scala.Predef.String] = hydra.lib.lists.concat2[scala.Predef.String](hydra.lib.lists.map[hydra.haskell.syntax.NamePart,
     scala.Predef.String](h)(qualifiers))(Seq(h(unqual)))
  hydra.lib.strings.intercalate(".")(allParts)
}
