package hydra.java.serde

import hydra.java.syntax.*

def escapeJavaChar(c: Int): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(34))("\\\"")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(92))("\\\\")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(10))("\\n")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(13))("\\r")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(9))("\\t")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(8))("\\b")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(12))("\\f")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.logic.and(hydra.lib.equality.gte[Int](c)(32))(hydra.lib.equality.lt[Int](c)(127)))(hydra.lib.strings.fromList(Seq(c)))(hydra.java.serde.javaUnicodeEscape(c)))))))))

def escapeJavaString(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.cat(hydra.lib.lists.map[Int, scala.Predef.String]((c: Int) => hydra.java.serde.escapeJavaChar(c))(hydra.lib.strings.toList(s)))

def hexDigit(n: Int): Int =
  hydra.lib.logic.ifElse[Int](hydra.lib.equality.lt[Int](n)(10))(hydra.lib.math.add(n)(48))(hydra.lib.math.add(hydra.lib.math.sub(n)(10))(65))

def javaFloatLiteralText(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](s)("NaN"))("Double.NaN")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](s)("Infinity"))("Double.POSITIVE_INFINITY")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](s)("-Infinity"))("Double.NEGATIVE_INFINITY")(s)))

def javaUnicodeEscape(n: Int): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.gt[Int](n)(65535))({
  lazy val `n_`: Int = hydra.lib.math.sub(n)(65536)
  {
    lazy val hi: Int = hydra.lib.math.add(55296)(hydra.lib.math.div(`n_`)(1024))
    {
      lazy val lo: Int = hydra.lib.math.add(56320)(hydra.lib.math.mod(`n_`)(1024))
      hydra.lib.strings.cat2(hydra.lib.strings.cat2("\\u")(hydra.java.serde.padHex4(hi)))(hydra.lib.strings.cat2("\\u")(hydra.java.serde.padHex4(lo)))
    }
  }
})(hydra.lib.strings.cat2("\\u")(hydra.java.serde.padHex4(n)))

def padHex4(n: Int): scala.Predef.String =
  {
  lazy val d3: Int = hydra.lib.math.div(n)(4096)
  lazy val r3: Int = hydra.lib.math.mod(n)(4096)
  lazy val d2: Int = hydra.lib.math.div(r3)(256)
  lazy val r2: Int = hydra.lib.math.mod(r3)(256)
  lazy val d1: Int = hydra.lib.math.div(r2)(16)
  lazy val d0: Int = hydra.lib.math.mod(r2)(16)
  hydra.lib.strings.fromList(Seq(hydra.java.serde.hexDigit(d3), hydra.java.serde.hexDigit(d2),
     hydra.java.serde.hexDigit(d1), hydra.java.serde.hexDigit(d0)))
}

def sanitizeJavaComment(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.intercalate("&gt;")(hydra.lib.strings.splitOn(">")(hydra.lib.strings.intercalate("&lt;")(hydra.lib.strings.splitOn("<")(s))))

def singleLineComment(c: scala.Predef.String): hydra.ast.Expr =
  hydra.serialization.cst(hydra.lib.strings.cat2("// ")(hydra.java.serde.sanitizeJavaComment(c)))

def withComments(mc: Option[scala.Predef.String])(expr: hydra.ast.Expr): hydra.ast.Expr =
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](expr)((c: scala.Predef.String) =>
  hydra.serialization.newlineSep(Seq(hydra.serialization.cst(hydra.lib.strings.cat2("/**\n")(hydra.lib.strings.cat2(hydra.lib.strings.intercalate("\n")(hydra.lib.lists.map[scala.Predef.String,
     scala.Predef.String]((l: scala.Predef.String) => hydra.lib.strings.cat2(" * ")(l))(hydra.lib.strings.lines(hydra.java.serde.sanitizeJavaComment(c)))))("\n */"))),
     expr)))(mc)

def writeAdditionalBound(ab: hydra.java.syntax.AdditionalBound): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("&"), hydra.java.serde.writeInterfaceType(ab)))

def writeAdditiveExpression(e: hydra.java.syntax.AdditiveExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.AdditiveExpression.unary(v_AdditiveExpression_unary_m) => hydra.java.serde.writeMultiplicativeExpression(v_AdditiveExpression_unary_m)
  case hydra.java.syntax.AdditiveExpression.plus(v_AdditiveExpression_plus_b) => hydra.serialization.infixWs("+")(hydra.java.serde.writeAdditiveExpression(v_AdditiveExpression_plus_b.lhs))(hydra.java.serde.writeMultiplicativeExpression(v_AdditiveExpression_plus_b.rhs))
  case hydra.java.syntax.AdditiveExpression.minus(v_AdditiveExpression_minus_b) => hydra.serialization.infixWs("-")(hydra.java.serde.writeAdditiveExpression(v_AdditiveExpression_minus_b.lhs))(hydra.java.serde.writeMultiplicativeExpression(v_AdditiveExpression_minus_b.rhs))

def writeAmbiguousName(an: hydra.java.syntax.AmbiguousName): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.java.syntax.Identifier, hydra.ast.Expr](hydra.java.serde.writeIdentifier)(an))

def writeAndExpression(ae: hydra.java.syntax.AndExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("&")(hydra.lib.lists.map[hydra.java.syntax.EqualityExpression,
     hydra.ast.Expr](hydra.java.serde.writeEqualityExpression)(ae))

def writeAnnotatedIdentifier(ai: hydra.java.syntax.AnnotatedIdentifier): hydra.ast.Expr = hydra.java.serde.writeIdentifier(ai.identifier)

def writeAnnotation(ann: hydra.java.syntax.Annotation): hydra.ast.Expr =
  ann match
  case hydra.java.syntax.Annotation.normal(v_Annotation_normal_n) => hydra.java.serde.writeNormalAnnotation(v_Annotation_normal_n)
  case hydra.java.syntax.Annotation.marker(v_Annotation_marker_m) => hydra.java.serde.writeMarkerAnnotation(v_Annotation_marker_m)
  case hydra.java.syntax.Annotation.singleElement(v_Annotation_singleElement_s) => hydra.java.serde.writeSingleElementAnnotation(v_Annotation_singleElement_s)

def writeAnnotationTypeDeclaration[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:AnnotationTypeDeclaration")

def writeArrayAccess[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ArrayAccess")

def writeArrayCreationExpression(ace: hydra.java.syntax.ArrayCreationExpression): hydra.ast.Expr =
  ace match
  case hydra.java.syntax.ArrayCreationExpression.primitiveArray(v_ArrayCreationExpression_primitiveArray_pa) => {
    lazy val pt: hydra.java.syntax.PrimitiveTypeWithAnnotations = (v_ArrayCreationExpression_primitiveArray_pa.`type`)
    lazy val ai: hydra.java.syntax.ArrayInitializer = (v_ArrayCreationExpression_primitiveArray_pa.array)
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("new"), hydra.serialization.noSep(Seq(hydra.java.serde.writePrimitiveTypeWithAnnotations(pt),
       hydra.serialization.cst("[]"))), hydra.java.serde.writeArrayInitializer(ai)))
  }
  case hydra.java.syntax.ArrayCreationExpression.classOrInterfaceArray(v_ArrayCreationExpression_classOrInterfaceArray__) => hydra.serialization.cst("STUB:ArrayCreationExpression")
  case hydra.java.syntax.ArrayCreationExpression.primitive(v_ArrayCreationExpression_primitive__) => hydra.serialization.cst("STUB:ArrayCreationExpression")
  case hydra.java.syntax.ArrayCreationExpression.classOrInterface(v_ArrayCreationExpression_classOrInterface__) => hydra.serialization.cst("STUB:ArrayCreationExpression")

def writeArrayInitializer(ai: hydra.java.syntax.ArrayInitializer): hydra.ast.Expr =
  {
  lazy val groups: Seq[Seq[hydra.java.syntax.VariableInitializer]] = ai
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.equal[Int](hydra.lib.lists.length[Seq[hydra.java.syntax.VariableInitializer]](groups))(1))(hydra.serialization.noSep(Seq(hydra.serialization.cst("{"),
     hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.VariableInitializer,
     hydra.ast.Expr](hydra.java.serde.writeVariableInitializer)(hydra.lib.lists.head[Seq[hydra.java.syntax.VariableInitializer]](groups))),
     hydra.serialization.cst("}"))))(hydra.serialization.cst("{}"))
}

def writeArrayType(at: hydra.java.syntax.ArrayType): hydra.ast.Expr =
  {
  lazy val dims: hydra.java.syntax.Dims = (at.dims)
  lazy val variant: hydra.java.syntax.ArrayType_Variant = (at.variant)
  lazy val varExpr: hydra.ast.Expr = variant match
    case hydra.java.syntax.ArrayType_Variant.primitive(v_ArrayType_Variant_primitive_pt) => hydra.java.serde.writePrimitiveTypeWithAnnotations(v_ArrayType_Variant_primitive_pt)
    case hydra.java.syntax.ArrayType_Variant.classOrInterface(v_ArrayType_Variant_classOrInterface_cit) => hydra.java.serde.writeClassOrInterfaceType(v_ArrayType_Variant_classOrInterface_cit)
    case hydra.java.syntax.ArrayType_Variant.variable(v_ArrayType_Variant_variable_tv) => hydra.java.serde.writeTypeVariable(v_ArrayType_Variant_variable_tv)
  hydra.serialization.noSep(Seq(varExpr, hydra.java.serde.writeDims(dims)))
}

def writeAssertStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:AssertStatement")

def writeAssignment(a: hydra.java.syntax.Assignment): hydra.ast.Expr =
  {
  lazy val lhs: hydra.java.syntax.LeftHandSide = (a.lhs)
  lazy val op: hydra.java.syntax.AssignmentOperator = (a.op)
  lazy val rhs: hydra.java.syntax.Expression = (a.expression)
  lazy val ctop: scala.Predef.String = op match
    case hydra.java.syntax.AssignmentOperator.simple => "="
    case hydra.java.syntax.AssignmentOperator.times => "*="
    case hydra.java.syntax.AssignmentOperator.div => "/="
    case hydra.java.syntax.AssignmentOperator.mod => "%="
    case hydra.java.syntax.AssignmentOperator.plus => "+="
    case hydra.java.syntax.AssignmentOperator.minus => "-="
    case hydra.java.syntax.AssignmentOperator.shiftLeft => "<<="
    case hydra.java.syntax.AssignmentOperator.shiftRight => ">>="
    case hydra.java.syntax.AssignmentOperator.shiftRightZeroFill => ">>>="
    case hydra.java.syntax.AssignmentOperator.and => "&="
    case hydra.java.syntax.AssignmentOperator.xor => "^="
    case hydra.java.syntax.AssignmentOperator.or => "|="
  hydra.serialization.infixWs(ctop)(hydra.java.serde.writeLeftHandSide(lhs))(hydra.java.serde.writeExpression(rhs))
}

def writeAssignmentExpression(e: hydra.java.syntax.AssignmentExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.AssignmentExpression.conditional(v_AssignmentExpression_conditional_c) => hydra.java.serde.writeConditionalExpression(v_AssignmentExpression_conditional_c)
  case hydra.java.syntax.AssignmentExpression.assignment(v_AssignmentExpression_assignment_a) => hydra.java.serde.writeAssignment(v_AssignmentExpression_assignment_a)

def writeBlock(b: hydra.java.syntax.Block): hydra.ast.Expr =
  hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.java.syntax.BlockStatement,
     hydra.ast.Expr](hydra.java.serde.writeBlockStatement)(b)))

def writeBlockStatement(s: hydra.java.syntax.BlockStatement): hydra.ast.Expr =
  s match
  case hydra.java.syntax.BlockStatement.localVariableDeclaration(v_BlockStatement_localVariableDeclaration_d) => hydra.java.serde.writeLocalVariableDeclarationStatement(v_BlockStatement_localVariableDeclaration_d)
  case hydra.java.syntax.BlockStatement.`class`(v_BlockStatement_class_cd) => hydra.java.serde.writeClassDeclaration(v_BlockStatement_class_cd)
  case hydra.java.syntax.BlockStatement.statement(v_BlockStatement_statement_s2) => hydra.java.serde.writeStatement(v_BlockStatement_statement_s2)

def writeBreakStatement(bs: hydra.java.syntax.BreakStatement): hydra.ast.Expr =
  {
  lazy val mlabel: Option[hydra.java.syntax.Identifier] = bs
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("break")),
     hydra.lib.maybes.map[hydra.java.syntax.Identifier, hydra.ast.Expr](hydra.java.serde.writeIdentifier)(mlabel)))))
}

def writeCastExpression(e: hydra.java.syntax.CastExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.CastExpression.primitive(v_CastExpression_primitive_p) => hydra.java.serde.writeCastExpression_Primitive(v_CastExpression_primitive_p)
  case hydra.java.syntax.CastExpression.notPlusMinus(v_CastExpression_notPlusMinus_npm) => hydra.java.serde.writeCastExpression_NotPlusMinus(v_CastExpression_notPlusMinus_npm)
  case hydra.java.syntax.CastExpression.lambda(v_CastExpression_lambda_l) => hydra.java.serde.writeCastExpression_Lambda(v_CastExpression_lambda_l)

def writeCastExpression_Lambda[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:CastExpression_Lambda")

def writeCastExpression_NotPlusMinus(npm: hydra.java.syntax.CastExpression_NotPlusMinus): hydra.ast.Expr =
  {
  lazy val rb: hydra.java.syntax.CastExpression_RefAndBounds = (npm.refAndBounds)
  lazy val ex: hydra.java.syntax.UnaryExpression = (npm.expression)
  hydra.serialization.spaceSep(Seq(hydra.java.serde.writeCastExpression_RefAndBounds(rb),
     hydra.java.serde.writeUnaryExpression(ex)))
}

def writeCastExpression_Primitive(cp: hydra.java.syntax.CastExpression_Primitive): hydra.ast.Expr =
  {
  lazy val pt: hydra.java.syntax.PrimitiveTypeWithAnnotations = (cp.`type`)
  lazy val ex: hydra.java.syntax.UnaryExpression = (cp.expression)
  hydra.serialization.spaceSep(Seq(hydra.serialization.parenList(false)(Seq(hydra.java.serde.writePrimitiveTypeWithAnnotations(pt))),
     hydra.java.serde.writeUnaryExpression(ex)))
}

def writeCastExpression_RefAndBounds(rab: hydra.java.syntax.CastExpression_RefAndBounds): hydra.ast.Expr =
  {
  lazy val rt: hydra.java.syntax.ReferenceType = (rab.`type`)
  lazy val adds: Seq[hydra.java.syntax.AdditionalBound] = (rab.bounds)
  hydra.serialization.parenList(false)(Seq(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.java.serde.writeReferenceType(rt)),
     hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.AdditionalBound](adds))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.AdditionalBound,
     hydra.ast.Expr](hydra.java.serde.writeAdditionalBound)(adds)))))))))
}

def writeClassBody(cb: hydra.java.syntax.ClassBody): hydra.ast.Expr =
  hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.serialization.doubleNewlineSep(hydra.lib.lists.map[hydra.java.syntax.ClassBodyDeclarationWithComments,
     hydra.ast.Expr](hydra.java.serde.writeClassBodyDeclarationWithComments)(cb)))

def writeClassBodyDeclaration(d: hydra.java.syntax.ClassBodyDeclaration): hydra.ast.Expr =
  d match
  case hydra.java.syntax.ClassBodyDeclaration.classMember(v_ClassBodyDeclaration_classMember_d2) => hydra.java.serde.writeClassMemberDeclaration(v_ClassBodyDeclaration_classMember_d2)
  case hydra.java.syntax.ClassBodyDeclaration.instanceInitializer(v_ClassBodyDeclaration_instanceInitializer_i) => hydra.java.serde.writeInstanceInitializer(v_ClassBodyDeclaration_instanceInitializer_i)
  case hydra.java.syntax.ClassBodyDeclaration.staticInitializer(v_ClassBodyDeclaration_staticInitializer_i) => hydra.java.serde.writeStaticInitializer(v_ClassBodyDeclaration_staticInitializer_i)
  case hydra.java.syntax.ClassBodyDeclaration.constructorDeclaration(v_ClassBodyDeclaration_constructorDeclaration_d2) => hydra.java.serde.writeConstructorDeclaration(v_ClassBodyDeclaration_constructorDeclaration_d2)

def writeClassBodyDeclarationWithComments(cbdwc: hydra.java.syntax.ClassBodyDeclarationWithComments): hydra.ast.Expr =
  {
  lazy val d: hydra.java.syntax.ClassBodyDeclaration = (cbdwc.value)
  lazy val mc: Option[scala.Predef.String] = (cbdwc.comments)
  hydra.java.serde.withComments(mc)(hydra.java.serde.writeClassBodyDeclaration(d))
}

def writeClassDeclaration(d: hydra.java.syntax.ClassDeclaration): hydra.ast.Expr =
  d match
  case hydra.java.syntax.ClassDeclaration.normal(v_ClassDeclaration_normal_nd) => hydra.java.serde.writeNormalClassDeclaration(v_ClassDeclaration_normal_nd)
  case hydra.java.syntax.ClassDeclaration.`enum`(v_ClassDeclaration_enum_ed) => hydra.java.serde.writeEnumDeclaration(v_ClassDeclaration_enum_ed)

def writeClassInstanceCreationExpression(cice: hydra.java.syntax.ClassInstanceCreationExpression): hydra.ast.Expr =
  {
  lazy val mqual: Option[hydra.java.syntax.ClassInstanceCreationExpression_Qualifier] = (cice.qualifier)
  lazy val e: hydra.java.syntax.UnqualifiedClassInstanceCreationExpression = (cice.expression)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.java.syntax.ClassInstanceCreationExpression_Qualifier](hydra.java.serde.writeUnqualifiedClassInstanceCreationExpression(e))((q: hydra.java.syntax.ClassInstanceCreationExpression_Qualifier) =>
    hydra.serialization.dotSep(Seq(hydra.java.serde.writeClassInstanceCreationExpression_Qualifier(q),
       hydra.java.serde.writeUnqualifiedClassInstanceCreationExpression(e))))(mqual)
}

def writeClassInstanceCreationExpression_Qualifier(q: hydra.java.syntax.ClassInstanceCreationExpression_Qualifier): hydra.ast.Expr =
  q match
  case hydra.java.syntax.ClassInstanceCreationExpression_Qualifier.expression(v_ClassInstanceCreationExpression_Qualifier_expression_en) => hydra.java.serde.writeExpressionName(v_ClassInstanceCreationExpression_Qualifier_expression_en)
  case hydra.java.syntax.ClassInstanceCreationExpression_Qualifier.primary(v_ClassInstanceCreationExpression_Qualifier_primary_p) => hydra.java.serde.writePrimary(v_ClassInstanceCreationExpression_Qualifier_primary_p)

def writeClassLiteral[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ClassLiteral")

def writeClassMemberDeclaration(d: hydra.java.syntax.ClassMemberDeclaration): hydra.ast.Expr =
  d match
  case hydra.java.syntax.ClassMemberDeclaration.field(v_ClassMemberDeclaration_field_fd) => hydra.java.serde.writeFieldDeclaration(v_ClassMemberDeclaration_field_fd)
  case hydra.java.syntax.ClassMemberDeclaration.method(v_ClassMemberDeclaration_method_md) => hydra.java.serde.writeMethodDeclaration(v_ClassMemberDeclaration_method_md)
  case hydra.java.syntax.ClassMemberDeclaration.`class`(v_ClassMemberDeclaration_class_cd) => hydra.java.serde.writeClassDeclaration(v_ClassMemberDeclaration_class_cd)
  case hydra.java.syntax.ClassMemberDeclaration.interface(v_ClassMemberDeclaration_interface_id) => hydra.java.serde.writeInterfaceDeclaration(v_ClassMemberDeclaration_interface_id)
  case hydra.java.syntax.ClassMemberDeclaration.none => hydra.serialization.cst(";")

def writeClassModifier(m: hydra.java.syntax.ClassModifier): hydra.ast.Expr =
  m match
  case hydra.java.syntax.ClassModifier.annotation(v_ClassModifier_annotation_ann) => hydra.java.serde.writeAnnotation(v_ClassModifier_annotation_ann)
  case hydra.java.syntax.ClassModifier.public => hydra.serialization.cst("public")
  case hydra.java.syntax.ClassModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.java.syntax.ClassModifier.`private` => hydra.serialization.cst("private")
  case hydra.java.syntax.ClassModifier.`abstract` => hydra.serialization.cst("abstract")
  case hydra.java.syntax.ClassModifier.static => hydra.serialization.cst("static")
  case hydra.java.syntax.ClassModifier.`final` => hydra.serialization.cst("final")
  case hydra.java.syntax.ClassModifier.strictfp => hydra.serialization.cst("strictfp")

def writeClassOrInterfaceType(cit: hydra.java.syntax.ClassOrInterfaceType): hydra.ast.Expr =
  cit match
  case hydra.java.syntax.ClassOrInterfaceType.`class`(v_ClassOrInterfaceType_class_ct) => hydra.java.serde.writeClassType(v_ClassOrInterfaceType_class_ct)
  case hydra.java.syntax.ClassOrInterfaceType.interface(v_ClassOrInterfaceType_interface_it) => hydra.java.serde.writeInterfaceType(v_ClassOrInterfaceType_interface_it)

def writeClassOrInterfaceTypeToInstantiate(coitti: hydra.java.syntax.ClassOrInterfaceTypeToInstantiate): hydra.ast.Expr =
  {
  lazy val ids: Seq[hydra.java.syntax.AnnotatedIdentifier] = (coitti.identifiers)
  lazy val margs: Option[hydra.java.syntax.TypeArgumentsOrDiamond] = (coitti.typeArguments)
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.dotSep(hydra.lib.lists.map[hydra.java.syntax.AnnotatedIdentifier,
     hydra.ast.Expr](hydra.java.serde.writeAnnotatedIdentifier)(ids))), hydra.lib.maybes.map[hydra.java.syntax.TypeArgumentsOrDiamond,
     hydra.ast.Expr](hydra.java.serde.writeTypeArgumentsOrDiamond)(margs))))
}

def writeClassType(ct: hydra.java.syntax.ClassType): hydra.ast.Expr =
  {
  lazy val anns: Seq[hydra.java.syntax.Annotation] = (ct.annotations)
  lazy val qual: hydra.java.syntax.ClassTypeQualifier = (ct.qualifier)
  lazy val id: hydra.java.syntax.TypeIdentifier = (ct.identifier)
  lazy val args: Seq[hydra.java.syntax.TypeArgument] = (ct.arguments)
  lazy val qualifiedId: hydra.ast.Expr = qual match
    case hydra.java.syntax.ClassTypeQualifier.none => hydra.java.serde.writeTypeIdentifier(id)
    case hydra.java.syntax.ClassTypeQualifier.`package`(v_ClassTypeQualifier_package_pkg) => hydra.serialization.dotSep(Seq(hydra.java.serde.writePackageName(v_ClassTypeQualifier_package_pkg),
       hydra.java.serde.writeTypeIdentifier(id)))
    case hydra.java.syntax.ClassTypeQualifier.parent(v_ClassTypeQualifier_parent_cit) => hydra.serialization.dotSep(Seq(hydra.java.serde.writeClassOrInterfaceType(v_ClassTypeQualifier_parent_cit),
       hydra.java.serde.writeTypeIdentifier(id)))
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.Annotation,
     hydra.ast.Expr](hydra.java.serde.writeAnnotation)(anns)))), Some(qualifiedId))))),
     hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.TypeArgument](args))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.TypeArgument,
     hydra.ast.Expr](hydra.java.serde.writeTypeArgument)(args)))))))
}

def writeCompilationUnit(u: hydra.java.syntax.CompilationUnit): hydra.ast.Expr =
  u match
  case hydra.java.syntax.CompilationUnit.ordinary(v_CompilationUnit_ordinary_ocu) => {
    lazy val mpkg: Option[hydra.java.syntax.PackageDeclaration] = (v_CompilationUnit_ordinary_ocu.`package`)
    lazy val imports: Seq[hydra.java.syntax.ImportDeclaration] = (v_CompilationUnit_ordinary_ocu.imports)
    lazy val types: Seq[hydra.java.syntax.TypeDeclarationWithComments] = (v_CompilationUnit_ordinary_ocu.types)
    lazy val warning: Option[hydra.ast.Expr] = Some(hydra.java.serde.singleLineComment(hydra.constants.warningAutoGeneratedFile))
    lazy val pkgSec: Option[hydra.ast.Expr] = hydra.lib.maybes.map[hydra.java.syntax.PackageDeclaration,
       hydra.ast.Expr](hydra.java.serde.writePackageDeclaration)(mpkg)
    lazy val importsSec: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.ImportDeclaration](imports))(None)(Some(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.java.syntax.ImportDeclaration,
       hydra.ast.Expr](hydra.java.serde.writeImportDeclaration)(imports))))
    lazy val typesSec: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.TypeDeclarationWithComments](types))(None)(Some(hydra.serialization.doubleNewlineSep(hydra.lib.lists.map[hydra.java.syntax.TypeDeclarationWithComments,
       hydra.ast.Expr](hydra.java.serde.writeTypeDeclarationWithComments)(types))))
    hydra.serialization.doubleNewlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(warning,
       pkgSec, importsSec, typesSec)))
  }

def writeConditionalAndExpression(cae: hydra.java.syntax.ConditionalAndExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("&&")(hydra.lib.lists.map[hydra.java.syntax.InclusiveOrExpression,
     hydra.ast.Expr](hydra.java.serde.writeInclusiveOrExpression)(cae))

def writeConditionalExpression(c: hydra.java.syntax.ConditionalExpression): hydra.ast.Expr =
  c match
  case hydra.java.syntax.ConditionalExpression.simple(v_ConditionalExpression_simple_co) => hydra.java.serde.writeConditionalOrExpression(v_ConditionalExpression_simple_co)
  case hydra.java.syntax.ConditionalExpression.ternaryCond(v_ConditionalExpression_ternaryCond_tc) => hydra.java.serde.writeConditionalExpression_TernaryCond(v_ConditionalExpression_ternaryCond_tc)
  case hydra.java.syntax.ConditionalExpression.ternaryLambda(v_ConditionalExpression_ternaryLambda_tl) => hydra.java.serde.writeConditionalExpression_TernaryLambda(v_ConditionalExpression_ternaryLambda_tl)

def writeConditionalExpression_TernaryCond[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ConditionalExpression_TernaryCond")

def writeConditionalExpression_TernaryLambda[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ConditionalExpression_TernaryLambda")

def writeConditionalOrExpression(coe: hydra.java.syntax.ConditionalOrExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("||")(hydra.lib.lists.map[hydra.java.syntax.ConditionalAndExpression,
     hydra.ast.Expr](hydra.java.serde.writeConditionalAndExpression)(coe))

def writeConstantDeclaration(cd: hydra.java.syntax.ConstantDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.ConstantModifier] = (cd.modifiers)
  lazy val typ: hydra.java.syntax.UnannType = (cd.`type`)
  lazy val vars: Seq[hydra.java.syntax.VariableDeclarator] = (cd.variables)
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.ConstantModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.ConstantModifier,
     hydra.ast.Expr](hydra.java.serde.writeConstantModifier)(mods)))), Some(hydra.java.serde.writeUnannType(typ)),
     Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.VariableDeclarator,
     hydra.ast.Expr](hydra.java.serde.writeVariableDeclarator)(vars)))))))
}

def writeConstantModifier[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ConstantModifier")

def writeConstructorBody(cb: hydra.java.syntax.ConstructorBody): hydra.ast.Expr =
  {
  lazy val minvoc: Option[hydra.java.syntax.ExplicitConstructorInvocation] = (cb.invocation)
  lazy val stmts: Seq[hydra.java.syntax.BlockStatement] = (cb.statements)
  hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.serialization.doubleNewlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.java.syntax.ExplicitConstructorInvocation,
     hydra.ast.Expr](hydra.java.serde.writeExplicitConstructorInvocation)(minvoc),
     Some(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.java.syntax.BlockStatement,
     hydra.ast.Expr](hydra.java.serde.writeBlockStatement)(stmts)))))))
}

def writeConstructorDeclaration(cd: hydra.java.syntax.ConstructorDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.ConstructorModifier] = (cd.modifiers)
  lazy val cons: hydra.java.syntax.ConstructorDeclarator = (cd.constructor)
  lazy val mthrows: Option[hydra.java.syntax.Throws] = (cd.throws)
  lazy val body: hydra.java.syntax.ConstructorBody = (cd.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.ConstructorModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.ConstructorModifier,
     hydra.ast.Expr](hydra.java.serde.writeConstructorModifier)(mods)))), Some(hydra.java.serde.writeConstructorDeclarator(cons)),
     hydra.lib.maybes.map[hydra.java.syntax.Throws, hydra.ast.Expr](hydra.java.serde.writeThrows)(mthrows),
     Some(hydra.java.serde.writeConstructorBody(body)))))
}

def writeConstructorDeclarator(cd: hydra.java.syntax.ConstructorDeclarator): hydra.ast.Expr =
  {
  lazy val tparams: Seq[hydra.java.syntax.TypeParameter] = (cd.parameters)
  lazy val name: hydra.java.syntax.SimpleTypeName = (cd.name)
  lazy val fparams: Seq[hydra.java.syntax.FormalParameter] = (cd.formalParameters)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
     hydra.ast.Expr](hydra.java.serde.writeTypeParameter)(tparams)))), Some(hydra.java.serde.writeSimpleTypeName(name)),
     Some(hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.java.syntax.FormalParameter,
     hydra.ast.Expr](hydra.java.serde.writeFormalParameter)(fparams))))))
}

def writeConstructorModifier(m: hydra.java.syntax.ConstructorModifier): hydra.ast.Expr =
  m match
  case hydra.java.syntax.ConstructorModifier.annotation(v_ConstructorModifier_annotation_ann) => hydra.java.serde.writeAnnotation(v_ConstructorModifier_annotation_ann)
  case hydra.java.syntax.ConstructorModifier.public => hydra.serialization.cst("public")
  case hydra.java.syntax.ConstructorModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.java.syntax.ConstructorModifier.`private` => hydra.serialization.cst("private")

def writeContinueStatement(cs: hydra.java.syntax.ContinueStatement): hydra.ast.Expr =
  {
  lazy val mlabel: Option[hydra.java.syntax.Identifier] = cs
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("continue")),
     hydra.lib.maybes.map[hydra.java.syntax.Identifier, hydra.ast.Expr](hydra.java.serde.writeIdentifier)(mlabel)))))
}

def writeDims(d: hydra.java.syntax.Dims): hydra.ast.Expr =
  hydra.serialization.noSep(hydra.lib.lists.map[Seq[hydra.java.syntax.Annotation],
     hydra.ast.Expr]((_x: Seq[hydra.java.syntax.Annotation]) => hydra.serialization.cst("[]"))(d))

def writeDoStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:DoStatement")

def writeElementValue(ev: hydra.java.syntax.ElementValue): hydra.ast.Expr =
  ev match
  case hydra.java.syntax.ElementValue.conditionalExpression(v_ElementValue_conditionalExpression_c) => hydra.java.serde.writeConditionalExpression(v_ElementValue_conditionalExpression_c)
  case hydra.java.syntax.ElementValue.elementValueArrayInitializer(v_ElementValue_elementValueArrayInitializer_evai) => hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.ElementValue,
     hydra.ast.Expr](hydra.java.serde.writeElementValue)(v_ElementValue_elementValueArrayInitializer_evai))
  case hydra.java.syntax.ElementValue.annotation(v_ElementValue_annotation_ann) => hydra.java.serde.writeAnnotation(v_ElementValue_annotation_ann)

def writeElementValuePair(evp: hydra.java.syntax.ElementValuePair): hydra.ast.Expr =
  {
  lazy val k: hydra.java.syntax.Identifier = (evp.key)
  lazy val v: hydra.java.syntax.ElementValue = (evp.value)
  hydra.serialization.infixWs("=")(hydra.java.serde.writeIdentifier(k))(hydra.java.serde.writeElementValue(v))
}

def writeEnumDeclaration[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:EnumDeclaration")

def writeEqualityExpression(e: hydra.java.syntax.EqualityExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.EqualityExpression.unary(v_EqualityExpression_unary_r) => hydra.java.serde.writeRelationalExpression(v_EqualityExpression_unary_r)
  case hydra.java.syntax.EqualityExpression.equal(v_EqualityExpression_equal_b) => hydra.serialization.infixWs("==")(hydra.java.serde.writeEqualityExpression(v_EqualityExpression_equal_b.lhs))(hydra.java.serde.writeRelationalExpression(v_EqualityExpression_equal_b.rhs))
  case hydra.java.syntax.EqualityExpression.notEqual(v_EqualityExpression_notEqual_b) => hydra.serialization.infixWs("!=")(hydra.java.serde.writeEqualityExpression(v_EqualityExpression_notEqual_b.lhs))(hydra.java.serde.writeRelationalExpression(v_EqualityExpression_notEqual_b.rhs))

def writeExclusiveOrExpression(eoe: hydra.java.syntax.ExclusiveOrExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("^")(hydra.lib.lists.map[hydra.java.syntax.AndExpression,
     hydra.ast.Expr](hydra.java.serde.writeAndExpression)(eoe))

def writeExplicitConstructorInvocation[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ExplicitConstructorInvocation")

def writeExpression(e: hydra.java.syntax.Expression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.Expression.lambda(v_Expression_lambda_l) => hydra.java.serde.writeLambdaExpression(v_Expression_lambda_l)
  case hydra.java.syntax.Expression.assignment(v_Expression_assignment_a) => hydra.java.serde.writeAssignmentExpression(v_Expression_assignment_a)

def writeExpressionName(en: hydra.java.syntax.ExpressionName): hydra.ast.Expr =
  {
  lazy val mqual: Option[hydra.java.syntax.AmbiguousName] = (en.qualifier)
  lazy val id: hydra.java.syntax.Identifier = (en.identifier)
  hydra.serialization.dotSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.java.syntax.AmbiguousName,
     hydra.ast.Expr](hydra.java.serde.writeAmbiguousName)(mqual), Some(hydra.java.serde.writeIdentifier(id)))))
}

def writeExpressionStatement(es: hydra.java.syntax.ExpressionStatement): hydra.ast.Expr = hydra.serialization.withSemi(hydra.java.serde.writeStatementExpression(es))

def writeFieldAccess(fa: hydra.java.syntax.FieldAccess): hydra.ast.Expr =
  {
  lazy val qual: hydra.java.syntax.FieldAccess_Qualifier = (fa.qualifier)
  lazy val id: hydra.java.syntax.Identifier = (fa.identifier)
  qual match
    case hydra.java.syntax.FieldAccess_Qualifier.primary(v_FieldAccess_Qualifier_primary_p) => hydra.serialization.dotSep(Seq(hydra.java.serde.writePrimary(v_FieldAccess_Qualifier_primary_p),
       hydra.java.serde.writeIdentifier(id)))
    case hydra.java.syntax.FieldAccess_Qualifier.`super` => hydra.serialization.dotSep(Seq(hydra.serialization.cst("super"),
       hydra.java.serde.writeIdentifier(id)))
    case hydra.java.syntax.FieldAccess_Qualifier.typed(v_FieldAccess_Qualifier_typed_tn) => hydra.serialization.dotSep(Seq(hydra.java.serde.writeTypeName(v_FieldAccess_Qualifier_typed_tn),
       hydra.serialization.cst("super"), hydra.java.serde.writeIdentifier(id)))
}

def writeFieldDeclaration(fd: hydra.java.syntax.FieldDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.FieldModifier] = (fd.modifiers)
  lazy val typ: hydra.java.syntax.UnannType = (fd.unannType)
  lazy val vars: Seq[hydra.java.syntax.VariableDeclarator] = (fd.variableDeclarators)
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.FieldModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.FieldModifier,
     hydra.ast.Expr](hydra.java.serde.writeFieldModifier)(mods)))), Some(hydra.java.serde.writeUnannType(typ)),
     Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.VariableDeclarator,
     hydra.ast.Expr](hydra.java.serde.writeVariableDeclarator)(vars)))))))
}

def writeFieldModifier(m: hydra.java.syntax.FieldModifier): hydra.ast.Expr =
  m match
  case hydra.java.syntax.FieldModifier.annotation(v_FieldModifier_annotation_ann) => hydra.java.serde.writeAnnotation(v_FieldModifier_annotation_ann)
  case hydra.java.syntax.FieldModifier.public => hydra.serialization.cst("public")
  case hydra.java.syntax.FieldModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.java.syntax.FieldModifier.`private` => hydra.serialization.cst("private")
  case hydra.java.syntax.FieldModifier.static => hydra.serialization.cst("static")
  case hydra.java.syntax.FieldModifier.`final` => hydra.serialization.cst("final")
  case hydra.java.syntax.FieldModifier.transient => hydra.serialization.cst("transient")
  case hydra.java.syntax.FieldModifier.volatile => hydra.serialization.cst("volatile")

def writeFloatingPointLiteral(fl: hydra.java.syntax.FloatingPointLiteral): hydra.ast.Expr =
  hydra.serialization.cst(hydra.java.serde.javaFloatLiteralText(hydra.lib.literals.showBigfloat(fl)))

def writeFloatingPointType(ft: hydra.java.syntax.FloatingPointType): hydra.ast.Expr =
  ft match
  case hydra.java.syntax.FloatingPointType.float => hydra.serialization.cst("float")
  case hydra.java.syntax.FloatingPointType.double => hydra.serialization.cst("double")

def writeForStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ForStatement")

def writeFormalParameter(p: hydra.java.syntax.FormalParameter): hydra.ast.Expr =
  p match
  case hydra.java.syntax.FormalParameter.simple(v_FormalParameter_simple_s) => hydra.java.serde.writeFormalParameter_Simple(v_FormalParameter_simple_s)
  case hydra.java.syntax.FormalParameter.variableArity(v_FormalParameter_variableArity_v) => hydra.java.serde.writeVariableArityParameter(v_FormalParameter_variableArity_v)

def writeFormalParameter_Simple(fps: hydra.java.syntax.FormalParameter_Simple): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.VariableModifier] = (fps.modifiers)
  lazy val typ: hydra.java.syntax.UnannType = (fps.`type`)
  lazy val id: hydra.java.syntax.VariableDeclaratorId = (fps.id)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.VariableModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.VariableModifier,
     hydra.ast.Expr](hydra.java.serde.writeVariableModifier)(mods)))), Some(hydra.java.serde.writeUnannType(typ)),
     Some(hydra.java.serde.writeVariableDeclaratorId(id)))))
}

def writeIdentifier(id: hydra.java.syntax.Identifier): hydra.ast.Expr = hydra.serialization.cst(id)

def writeIfThenElseStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:IfThenElseStatement")

def writeIfThenStatement(its: hydra.java.syntax.IfThenStatement): hydra.ast.Expr =
  {
  lazy val cond: hydra.java.syntax.Expression = (its.expression)
  lazy val thn: hydra.java.syntax.Statement = (its.statement)
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("if"), hydra.serialization.parenList(false)(Seq(hydra.java.serde.writeExpression(cond))),
     hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.java.serde.writeStatement(thn))))
}

def writeImportDeclaration(imp: hydra.java.syntax.ImportDeclaration): hydra.ast.Expr =
  imp match
  case hydra.java.syntax.ImportDeclaration.singleType(v_ImportDeclaration_singleType_st) => hydra.serialization.withSemi(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("import"),
     hydra.java.serde.writeTypeName(v_ImportDeclaration_singleType_st))))
  case hydra.java.syntax.ImportDeclaration.typeImportOnDemand(v_ImportDeclaration_typeImportOnDemand__) => hydra.serialization.cst("STUB:ImportDeclarationTypeImportOnDemand")
  case hydra.java.syntax.ImportDeclaration.singleStaticImport(v_ImportDeclaration_singleStaticImport__) => hydra.serialization.cst("STUB:ImportDeclarationSingleStaticImport")
  case hydra.java.syntax.ImportDeclaration.staticImportOnDemand(v_ImportDeclaration_staticImportOnDemand__) => hydra.serialization.cst("STUB:ImportDeclarationStaticImportOnDemand")

def writeInclusiveOrExpression(ioe: hydra.java.syntax.InclusiveOrExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("|")(hydra.lib.lists.map[hydra.java.syntax.ExclusiveOrExpression,
     hydra.ast.Expr](hydra.java.serde.writeExclusiveOrExpression)(ioe))

def writeInstanceInitializer[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:InstanceInitializer")

def writeIntegerLiteral(il: hydra.java.syntax.IntegerLiteral): hydra.ast.Expr =
  {
  lazy val i: BigInt = il
  lazy val suffix: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.logic.or(hydra.lib.equality.gt[BigInt](i)(BigInt("2147483647")))(hydra.lib.equality.lt[BigInt](i)(BigInt("-2147483648"))))("L")("")
  hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.showBigint(i))(suffix))
}

def writeIntegralType(t: hydra.java.syntax.IntegralType): hydra.ast.Expr =
  t match
  case hydra.java.syntax.IntegralType.byte => hydra.serialization.cst("byte")
  case hydra.java.syntax.IntegralType.short => hydra.serialization.cst("short")
  case hydra.java.syntax.IntegralType.int => hydra.serialization.cst("int")
  case hydra.java.syntax.IntegralType.long => hydra.serialization.cst("long")
  case hydra.java.syntax.IntegralType.char => hydra.serialization.cst("char")

def writeInterfaceBody(ib: hydra.java.syntax.InterfaceBody): hydra.ast.Expr =
  hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.serialization.doubleNewlineSep(hydra.lib.lists.map[hydra.java.syntax.InterfaceMemberDeclaration,
     hydra.ast.Expr](hydra.java.serde.writeInterfaceMemberDeclaration)(ib)))

def writeInterfaceDeclaration(d: hydra.java.syntax.InterfaceDeclaration): hydra.ast.Expr =
  d match
  case hydra.java.syntax.InterfaceDeclaration.normalInterface(v_InterfaceDeclaration_normalInterface_n) => hydra.java.serde.writeNormalInterfaceDeclaration(v_InterfaceDeclaration_normalInterface_n)
  case hydra.java.syntax.InterfaceDeclaration.annotationType(v_InterfaceDeclaration_annotationType_a) => hydra.java.serde.writeAnnotationTypeDeclaration(v_InterfaceDeclaration_annotationType_a)

def writeInterfaceMemberDeclaration(d: hydra.java.syntax.InterfaceMemberDeclaration): hydra.ast.Expr =
  d match
  case hydra.java.syntax.InterfaceMemberDeclaration.constant(v_InterfaceMemberDeclaration_constant_c) => hydra.java.serde.writeConstantDeclaration(v_InterfaceMemberDeclaration_constant_c)
  case hydra.java.syntax.InterfaceMemberDeclaration.interfaceMethod(v_InterfaceMemberDeclaration_interfaceMethod_im) => hydra.java.serde.writeInterfaceMethodDeclaration(v_InterfaceMemberDeclaration_interfaceMethod_im)
  case hydra.java.syntax.InterfaceMemberDeclaration.`class`(v_InterfaceMemberDeclaration_class_cd) => hydra.java.serde.writeClassDeclaration(v_InterfaceMemberDeclaration_class_cd)
  case hydra.java.syntax.InterfaceMemberDeclaration.interface(v_InterfaceMemberDeclaration_interface_id) => hydra.java.serde.writeInterfaceDeclaration(v_InterfaceMemberDeclaration_interface_id)

def writeInterfaceMethodDeclaration(imd: hydra.java.syntax.InterfaceMethodDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.InterfaceMethodModifier] = (imd.modifiers)
  lazy val header: hydra.java.syntax.MethodHeader = (imd.header)
  lazy val body: hydra.java.syntax.MethodBody = (imd.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.InterfaceMethodModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.InterfaceMethodModifier,
     hydra.ast.Expr](hydra.java.serde.writeInterfaceMethodModifier)(mods)))), Some(hydra.java.serde.writeMethodHeader(header)),
     Some(hydra.java.serde.writeMethodBody(body)))))
}

def writeInterfaceMethodModifier(m: hydra.java.syntax.InterfaceMethodModifier): hydra.ast.Expr =
  m match
  case hydra.java.syntax.InterfaceMethodModifier.annotation(v_InterfaceMethodModifier_annotation_a) => hydra.java.serde.writeAnnotation(v_InterfaceMethodModifier_annotation_a)
  case hydra.java.syntax.InterfaceMethodModifier.public => hydra.serialization.cst("public")
  case hydra.java.syntax.InterfaceMethodModifier.`private` => hydra.serialization.cst("private")
  case hydra.java.syntax.InterfaceMethodModifier.`abstract` => hydra.serialization.cst("abstract")
  case hydra.java.syntax.InterfaceMethodModifier.default => hydra.serialization.cst("default")
  case hydra.java.syntax.InterfaceMethodModifier.static => hydra.serialization.cst("static")
  case hydra.java.syntax.InterfaceMethodModifier.strictfp => hydra.serialization.cst("strictfp")

def writeInterfaceModifier(m: hydra.java.syntax.InterfaceModifier): hydra.ast.Expr =
  m match
  case hydra.java.syntax.InterfaceModifier.annotation(v_InterfaceModifier_annotation_a) => hydra.java.serde.writeAnnotation(v_InterfaceModifier_annotation_a)
  case hydra.java.syntax.InterfaceModifier.public => hydra.serialization.cst("public")
  case hydra.java.syntax.InterfaceModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.java.syntax.InterfaceModifier.`private` => hydra.serialization.cst("private")
  case hydra.java.syntax.InterfaceModifier.`abstract` => hydra.serialization.cst("abstract")
  case hydra.java.syntax.InterfaceModifier.static => hydra.serialization.cst("static")
  case hydra.java.syntax.InterfaceModifier.strictfb => hydra.serialization.cst("strictfb")

def writeInterfaceType(it: hydra.java.syntax.InterfaceType): hydra.ast.Expr = hydra.java.serde.writeClassType(it)

def writeLabeledStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:LabeledStatement")

def writeLambdaBody(b: hydra.java.syntax.LambdaBody): hydra.ast.Expr =
  b match
  case hydra.java.syntax.LambdaBody.expression(v_LambdaBody_expression_e) => hydra.java.serde.writeExpression(v_LambdaBody_expression_e)
  case hydra.java.syntax.LambdaBody.block(v_LambdaBody_block_b2) => hydra.java.serde.writeBlock(v_LambdaBody_block_b2)

def writeLambdaExpression(le: hydra.java.syntax.LambdaExpression): hydra.ast.Expr =
  {
  lazy val params: hydra.java.syntax.LambdaParameters = (le.parameters)
  lazy val body: hydra.java.syntax.LambdaBody = (le.body)
  hydra.serialization.infixWs("->")(hydra.java.serde.writeLambdaParameters(params))(hydra.java.serde.writeLambdaBody(body))
}

def writeLambdaParameters(p: hydra.java.syntax.LambdaParameters): hydra.ast.Expr =
  p match
  case hydra.java.syntax.LambdaParameters.tuple(v_LambdaParameters_tuple_l) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.java.syntax.LambdaParameters,
     hydra.ast.Expr](hydra.java.serde.writeLambdaParameters)(v_LambdaParameters_tuple_l))
  case hydra.java.syntax.LambdaParameters.single(v_LambdaParameters_single_id) => hydra.java.serde.writeIdentifier(v_LambdaParameters_single_id)

def writeLeftHandSide(lhs: hydra.java.syntax.LeftHandSide): hydra.ast.Expr =
  lhs match
  case hydra.java.syntax.LeftHandSide.expressionName(v_LeftHandSide_expressionName_en) => hydra.java.serde.writeExpressionName(v_LeftHandSide_expressionName_en)
  case hydra.java.syntax.LeftHandSide.fieldAccess(v_LeftHandSide_fieldAccess_fa) => hydra.java.serde.writeFieldAccess(v_LeftHandSide_fieldAccess_fa)
  case hydra.java.syntax.LeftHandSide.arrayAccess(v_LeftHandSide_arrayAccess_aa) => hydra.java.serde.writeArrayAccess(v_LeftHandSide_arrayAccess_aa)

def writeLiteral(l: hydra.java.syntax.Literal): hydra.ast.Expr =
  l match
  case hydra.java.syntax.Literal.`null` => hydra.serialization.cst("null")
  case hydra.java.syntax.Literal.integer(v_Literal_integer_il) => hydra.java.serde.writeIntegerLiteral(v_Literal_integer_il)
  case hydra.java.syntax.Literal.floatingPoint(v_Literal_floatingPoint_fl) => hydra.java.serde.writeFloatingPointLiteral(v_Literal_floatingPoint_fl)
  case hydra.java.syntax.Literal.boolean(v_Literal_boolean_b) => hydra.serialization.cst(hydra.lib.logic.ifElse[scala.Predef.String](v_Literal_boolean_b)("true")("false"))
  case hydra.java.syntax.Literal.character(v_Literal_character_c) => {
    lazy val ci: Int = hydra.lib.literals.bigintToInt32(hydra.lib.literals.uint16ToBigint(v_Literal_character_c))
    hydra.serialization.cst(hydra.lib.strings.cat2("'")(hydra.lib.strings.cat2(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(39))("\\'")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(92))("\\\\")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(10))("\\n")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(13))("\\r")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(9))("\\t")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.logic.and(hydra.lib.equality.gte[Int](ci)(32))(hydra.lib.equality.lt[Int](ci)(127)))(hydra.lib.strings.fromList(Seq(ci)))(hydra.java.serde.javaUnicodeEscape(ci))))))))("'")))
  }
  case hydra.java.syntax.Literal.string(v_Literal_string_sl) => hydra.java.serde.writeStringLiteral(v_Literal_string_sl)

def writeLocalName(t: hydra.java.syntax.LocalVariableType): hydra.ast.Expr =
  t match
  case hydra.java.syntax.LocalVariableType.`type`(v_LocalVariableType_type_ut) => hydra.java.serde.writeUnannType(v_LocalVariableType_type_ut)
  case hydra.java.syntax.LocalVariableType.`var` => hydra.serialization.cst("var")

def writeLocalVariableDeclaration(lvd: hydra.java.syntax.LocalVariableDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.VariableModifier] = (lvd.modifiers)
  lazy val t: hydra.java.syntax.LocalVariableType = (lvd.`type`)
  lazy val decls: Seq[hydra.java.syntax.VariableDeclarator] = (lvd.declarators)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.VariableModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.VariableModifier,
     hydra.ast.Expr](hydra.java.serde.writeVariableModifier)(mods)))), Some(hydra.java.serde.writeLocalName(t)),
     Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.VariableDeclarator,
     hydra.ast.Expr](hydra.java.serde.writeVariableDeclarator)(decls))))))
}

def writeLocalVariableDeclarationStatement(lvds: hydra.java.syntax.LocalVariableDeclarationStatement): hydra.ast.Expr =
  hydra.serialization.withSemi(hydra.java.serde.writeLocalVariableDeclaration(lvds))

def writeMarkerAnnotation(ma: hydra.java.syntax.MarkerAnnotation): hydra.ast.Expr = hydra.serialization.prefix("@")(hydra.java.serde.writeTypeName(ma))

def writeMethodBody(b: hydra.java.syntax.MethodBody): hydra.ast.Expr =
  b match
  case hydra.java.syntax.MethodBody.block(v_MethodBody_block_block) => hydra.java.serde.writeBlock(v_MethodBody_block_block)
  case hydra.java.syntax.MethodBody.none => hydra.serialization.cst(";")

def writeMethodDeclaration(md: hydra.java.syntax.MethodDeclaration): hydra.ast.Expr =
  {
  lazy val anns: Seq[hydra.java.syntax.Annotation] = (md.annotations)
  lazy val mods: Seq[hydra.java.syntax.MethodModifier] = (md.modifiers)
  lazy val header: hydra.java.syntax.MethodHeader = (md.header)
  lazy val body: hydra.java.syntax.MethodBody = (md.body)
  lazy val headerAndBody: hydra.ast.Expr = hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.MethodModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.MethodModifier,
     hydra.ast.Expr](hydra.java.serde.writeMethodModifier)(mods)))), Some(hydra.java.serde.writeMethodHeader(header)),
     Some(hydra.java.serde.writeMethodBody(body)))))
  hydra.serialization.newlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.java.syntax.Annotation,
     hydra.ast.Expr](hydra.java.serde.writeAnnotation)(anns)))), Some(headerAndBody))))
}

def writeMethodDeclarator(md: hydra.java.syntax.MethodDeclarator): hydra.ast.Expr =
  {
  lazy val id: hydra.java.syntax.Identifier = (md.identifier)
  lazy val params: Seq[hydra.java.syntax.FormalParameter] = (md.formalParameters)
  hydra.serialization.noSep(Seq(hydra.java.serde.writeIdentifier(id), hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.java.syntax.FormalParameter,
     hydra.ast.Expr](hydra.java.serde.writeFormalParameter)(params))))
}

def writeMethodHeader(mh: hydra.java.syntax.MethodHeader): hydra.ast.Expr =
  {
  lazy val params: Seq[hydra.java.syntax.TypeParameter] = (mh.parameters)
  lazy val result: hydra.java.syntax.Result = (mh.result)
  lazy val decl: hydra.java.syntax.MethodDeclarator = (mh.declarator)
  lazy val mthrows: Option[hydra.java.syntax.Throws] = (mh.throws)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.TypeParameter](params))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
     hydra.ast.Expr](hydra.java.serde.writeTypeParameter)(params)))), Some(hydra.java.serde.writeResult(result)),
     Some(hydra.java.serde.writeMethodDeclarator(decl)), hydra.lib.maybes.map[hydra.java.syntax.Throws,
     hydra.ast.Expr](hydra.java.serde.writeThrows)(mthrows))))
}

def writeMethodInvocation(mi: hydra.java.syntax.MethodInvocation): hydra.ast.Expr =
  {
  lazy val header: hydra.java.syntax.MethodInvocation_Header = (mi.header)
  lazy val args: Seq[hydra.java.syntax.Expression] = (mi.arguments)
  lazy val argSec: hydra.ast.Expr = hydra.serialization.parenList(true)(hydra.lib.lists.map[hydra.java.syntax.Expression,
     hydra.ast.Expr](hydra.java.serde.writeExpression)(args))
  lazy val headerSec: hydra.ast.Expr = header match
    case hydra.java.syntax.MethodInvocation_Header.simple(v_MethodInvocation_Header_simple_mname) => hydra.java.serde.writeMethodName(v_MethodInvocation_Header_simple_mname)
    case hydra.java.syntax.MethodInvocation_Header.complex(v_MethodInvocation_Header_complex_cx) => {
      lazy val cvar: hydra.java.syntax.MethodInvocation_Variant = (v_MethodInvocation_Header_complex_cx.variant)
      lazy val targs: Seq[hydra.java.syntax.TypeArgument] = (v_MethodInvocation_Header_complex_cx.typeArguments)
      lazy val cid: hydra.java.syntax.Identifier = (v_MethodInvocation_Header_complex_cx.identifier)
      lazy val idSec: hydra.ast.Expr = hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.TypeArgument](targs))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.TypeArgument,
         hydra.ast.Expr](hydra.java.serde.writeTypeArgument)(targs)))), Some(hydra.java.serde.writeIdentifier(cid)))))
      cvar match
        case hydra.java.syntax.MethodInvocation_Variant.`type`(v_MethodInvocation_Variant_type_tname) => hydra.serialization.dotSep(Seq(hydra.java.serde.writeTypeName(v_MethodInvocation_Variant_type_tname),
           idSec))
        case hydra.java.syntax.MethodInvocation_Variant.expression(v_MethodInvocation_Variant_expression_en) => hydra.serialization.dotSep(Seq(hydra.java.serde.writeExpressionName(v_MethodInvocation_Variant_expression_en),
           idSec))
        case hydra.java.syntax.MethodInvocation_Variant.primary(v_MethodInvocation_Variant_primary_p) => hydra.serialization.dotSep(Seq(hydra.java.serde.writePrimary(v_MethodInvocation_Variant_primary_p),
           idSec))
        case hydra.java.syntax.MethodInvocation_Variant.`super` => hydra.serialization.dotSep(Seq(hydra.serialization.cst("super"),
           idSec))
        case hydra.java.syntax.MethodInvocation_Variant.typeSuper(v_MethodInvocation_Variant_typeSuper_tname) => hydra.serialization.dotSep(Seq(hydra.java.serde.writeTypeName(v_MethodInvocation_Variant_typeSuper_tname),
           hydra.serialization.cst("super"), idSec))
    }
  hydra.serialization.noSep(Seq(headerSec, argSec))
}

def writeMethodModifier(m: hydra.java.syntax.MethodModifier): hydra.ast.Expr =
  m match
  case hydra.java.syntax.MethodModifier.annotation(v_MethodModifier_annotation_ann) => hydra.java.serde.writeAnnotation(v_MethodModifier_annotation_ann)
  case hydra.java.syntax.MethodModifier.public => hydra.serialization.cst("public")
  case hydra.java.syntax.MethodModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.java.syntax.MethodModifier.`private` => hydra.serialization.cst("private")
  case hydra.java.syntax.MethodModifier.`abstract` => hydra.serialization.cst("abstract")
  case hydra.java.syntax.MethodModifier.`final` => hydra.serialization.cst("final")
  case hydra.java.syntax.MethodModifier.synchronized => hydra.serialization.cst("synchronized")
  case hydra.java.syntax.MethodModifier.native => hydra.serialization.cst("native")
  case hydra.java.syntax.MethodModifier.strictfb => hydra.serialization.cst("strictfb")

def writeMethodName(mn: hydra.java.syntax.MethodName): hydra.ast.Expr = hydra.java.serde.writeIdentifier(mn)

def writeMethodReference[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:MethodReference")

def writeMultiplicativeExpression(e: hydra.java.syntax.MultiplicativeExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.MultiplicativeExpression.unary(v_MultiplicativeExpression_unary_u) => hydra.java.serde.writeUnaryExpression(v_MultiplicativeExpression_unary_u)
  case hydra.java.syntax.MultiplicativeExpression.times(v_MultiplicativeExpression_times_b) => hydra.serialization.infixWs("*")(hydra.java.serde.writeMultiplicativeExpression(v_MultiplicativeExpression_times_b.lhs))(hydra.java.serde.writeUnaryExpression(v_MultiplicativeExpression_times_b.rhs))
  case hydra.java.syntax.MultiplicativeExpression.divide(v_MultiplicativeExpression_divide_b) => hydra.serialization.infixWs("/")(hydra.java.serde.writeMultiplicativeExpression(v_MultiplicativeExpression_divide_b.lhs))(hydra.java.serde.writeUnaryExpression(v_MultiplicativeExpression_divide_b.rhs))
  case hydra.java.syntax.MultiplicativeExpression.mod(v_MultiplicativeExpression_mod_b) => hydra.serialization.infixWs("%")(hydra.java.serde.writeMultiplicativeExpression(v_MultiplicativeExpression_mod_b.lhs))(hydra.java.serde.writeUnaryExpression(v_MultiplicativeExpression_mod_b.rhs))

def writeNormalAnnotation(na: hydra.java.syntax.NormalAnnotation): hydra.ast.Expr =
  {
  lazy val tname: hydra.java.syntax.TypeName = (na.typeName)
  lazy val pairs: Seq[hydra.java.syntax.ElementValuePair] = (na.pairs)
  hydra.serialization.prefix("@")(hydra.serialization.noSep(Seq(hydra.java.serde.writeTypeName(tname),
     hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.ElementValuePair,
     hydra.ast.Expr](hydra.java.serde.writeElementValuePair)(pairs)))))
}

def writeNormalClassDeclaration(ncd: hydra.java.syntax.NormalClassDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.ClassModifier] = (ncd.modifiers)
  lazy val id: hydra.java.syntax.TypeIdentifier = (ncd.identifier)
  lazy val tparams: Seq[hydra.java.syntax.TypeParameter] = (ncd.parameters)
  lazy val msuperc: Option[hydra.java.syntax.ClassType] = (ncd.`extends`)
  lazy val superi: Seq[hydra.java.syntax.InterfaceType] = (ncd.implements)
  lazy val body: hydra.java.syntax.ClassBody = (ncd.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.ClassModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.ClassModifier,
     hydra.ast.Expr](hydra.java.serde.writeClassModifier)(mods)))), Some(hydra.serialization.cst("class")),
     Some(hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.java.serde.writeTypeIdentifier(id)),
     hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
     hydra.ast.Expr](hydra.java.serde.writeTypeParameter)(tparams)))))))), hydra.lib.maybes.map[hydra.java.syntax.ClassType,
     hydra.ast.Expr]((c: hydra.java.syntax.ClassType) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("extends"), hydra.java.serde.writeClassType(c))))(msuperc),
       hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.InterfaceType](superi))(None)(Some(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("implements"),
       hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.InterfaceType,
       hydra.ast.Expr](hydra.java.serde.writeInterfaceType)(superi)))))), Some(hydra.java.serde.writeClassBody(body)))))
}

def writeNormalInterfaceDeclaration(nid: hydra.java.syntax.NormalInterfaceDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.InterfaceModifier] = (nid.modifiers)
  lazy val id: hydra.java.syntax.TypeIdentifier = (nid.identifier)
  lazy val tparams: Seq[hydra.java.syntax.TypeParameter] = (nid.parameters)
  lazy val `extends`: Seq[hydra.java.syntax.InterfaceType] = (nid.`extends`)
  lazy val body: hydra.java.syntax.InterfaceBody = (nid.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.InterfaceModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.InterfaceModifier,
     hydra.ast.Expr](hydra.java.serde.writeInterfaceModifier)(mods)))), Some(hydra.serialization.cst("interface")),
     Some(hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.java.serde.writeTypeIdentifier(id)),
     hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
     hydra.ast.Expr](hydra.java.serde.writeTypeParameter)(tparams)))))))), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.InterfaceType](`extends`))(None)(Some(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("extends"),
     hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.InterfaceType,
     hydra.ast.Expr](hydra.java.serde.writeInterfaceType)(`extends`)))))), Some(hydra.java.serde.writeInterfaceBody(body)))))
}

def writeNumericType(nt: hydra.java.syntax.NumericType): hydra.ast.Expr =
  nt match
  case hydra.java.syntax.NumericType.integral(v_NumericType_integral_it) => hydra.java.serde.writeIntegralType(v_NumericType_integral_it)
  case hydra.java.syntax.NumericType.floatingPoint(v_NumericType_floatingPoint_ft) => hydra.java.serde.writeFloatingPointType(v_NumericType_floatingPoint_ft)

def writePackageDeclaration(pd: hydra.java.syntax.PackageDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.PackageModifier] = (pd.modifiers)
  lazy val ids: Seq[hydra.java.syntax.Identifier] = (pd.identifiers)
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.PackageModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.PackageModifier,
     hydra.ast.Expr](hydra.java.serde.writePackageModifier)(mods)))), Some(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("package"),
     hydra.serialization.cst(hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[hydra.java.syntax.Identifier,
     scala.Predef.String]((id: hydra.java.syntax.Identifier) => id)(ids))))))))))
}

def writePackageModifier(pm: hydra.java.syntax.PackageModifier): hydra.ast.Expr = hydra.java.serde.writeAnnotation(pm)

def writePackageName(pn: hydra.java.syntax.PackageName): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.java.syntax.Identifier, hydra.ast.Expr](hydra.java.serde.writeIdentifier)(pn))

def writePackageOrTypeName(potn: hydra.java.syntax.PackageOrTypeName): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.java.syntax.Identifier, hydra.ast.Expr](hydra.java.serde.writeIdentifier)(potn))

def writePostDecrementExpression[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:PostDecrementExpression")

def writePostIncrementExpression[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:PostIncrementExpression")

def writePostfixExpression(e: hydra.java.syntax.PostfixExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.PostfixExpression.primary(v_PostfixExpression_primary_p) => hydra.java.serde.writePrimary(v_PostfixExpression_primary_p)
  case hydra.java.syntax.PostfixExpression.name(v_PostfixExpression_name_en) => hydra.java.serde.writeExpressionName(v_PostfixExpression_name_en)
  case hydra.java.syntax.PostfixExpression.postIncrement(v_PostfixExpression_postIncrement_pi) => hydra.java.serde.writePostIncrementExpression(v_PostfixExpression_postIncrement_pi)
  case hydra.java.syntax.PostfixExpression.postDecrement(v_PostfixExpression_postDecrement_pd) => hydra.java.serde.writePostDecrementExpression(v_PostfixExpression_postDecrement_pd)

def writePreDecrementExpression[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:PreDecrementExpression")

def writePreIncrementExpression[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:PreIncrementExpression")

def writePrimary(p: hydra.java.syntax.Primary): hydra.ast.Expr =
  p match
  case hydra.java.syntax.Primary.noNewArray(v_Primary_noNewArray_n) => hydra.java.serde.writePrimaryNoNewArrayExpressionExpression(v_Primary_noNewArray_n)
  case hydra.java.syntax.Primary.arrayCreation(v_Primary_arrayCreation_a) => hydra.java.serde.writeArrayCreationExpression(v_Primary_arrayCreation_a)

def writePrimaryNoNewArrayExpressionExpression(p: hydra.java.syntax.PrimaryNoNewArrayExpression): hydra.ast.Expr =
  p match
  case hydra.java.syntax.PrimaryNoNewArrayExpression.literal(v_PrimaryNoNewArrayExpression_literal_l) => hydra.java.serde.writeLiteral(v_PrimaryNoNewArrayExpression_literal_l)
  case hydra.java.syntax.PrimaryNoNewArrayExpression.classLiteral(v_PrimaryNoNewArrayExpression_classLiteral_cl) => hydra.java.serde.writeClassLiteral(v_PrimaryNoNewArrayExpression_classLiteral_cl)
  case hydra.java.syntax.PrimaryNoNewArrayExpression.`this` => hydra.serialization.cst("this")
  case hydra.java.syntax.PrimaryNoNewArrayExpression.dotThis(v_PrimaryNoNewArrayExpression_dotThis_n) => hydra.serialization.dotSep(Seq(hydra.java.serde.writeTypeName(v_PrimaryNoNewArrayExpression_dotThis_n),
     hydra.serialization.cst("this")))
  case hydra.java.syntax.PrimaryNoNewArrayExpression.parens(v_PrimaryNoNewArrayExpression_parens_e) => hydra.serialization.parenList(false)(Seq(hydra.java.serde.writeExpression(v_PrimaryNoNewArrayExpression_parens_e)))
  case hydra.java.syntax.PrimaryNoNewArrayExpression.classInstance(v_PrimaryNoNewArrayExpression_classInstance_ci) => hydra.java.serde.writeClassInstanceCreationExpression(v_PrimaryNoNewArrayExpression_classInstance_ci)
  case hydra.java.syntax.PrimaryNoNewArrayExpression.fieldAccess(v_PrimaryNoNewArrayExpression_fieldAccess_fa) => hydra.java.serde.writeFieldAccess(v_PrimaryNoNewArrayExpression_fieldAccess_fa)
  case hydra.java.syntax.PrimaryNoNewArrayExpression.arrayAccess(v_PrimaryNoNewArrayExpression_arrayAccess_aa) => hydra.java.serde.writeArrayAccess(v_PrimaryNoNewArrayExpression_arrayAccess_aa)
  case hydra.java.syntax.PrimaryNoNewArrayExpression.methodInvocation(v_PrimaryNoNewArrayExpression_methodInvocation_mi) => hydra.java.serde.writeMethodInvocation(v_PrimaryNoNewArrayExpression_methodInvocation_mi)
  case hydra.java.syntax.PrimaryNoNewArrayExpression.methodReference(v_PrimaryNoNewArrayExpression_methodReference_mr) => hydra.java.serde.writeMethodReference(v_PrimaryNoNewArrayExpression_methodReference_mr)

def writePrimitiveType(pt: hydra.java.syntax.PrimitiveType): hydra.ast.Expr =
  pt match
  case hydra.java.syntax.PrimitiveType.numeric(v_PrimitiveType_numeric_nt) => hydra.java.serde.writeNumericType(v_PrimitiveType_numeric_nt)
  case hydra.java.syntax.PrimitiveType.boolean => hydra.serialization.cst("boolean")

def writePrimitiveTypeWithAnnotations(ptwa: hydra.java.syntax.PrimitiveTypeWithAnnotations): hydra.ast.Expr =
  {
  lazy val pt: hydra.java.syntax.PrimitiveType = (ptwa.`type`)
  lazy val anns: Seq[hydra.java.syntax.Annotation] = (ptwa.annotations)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.Annotation,
     hydra.ast.Expr](hydra.java.serde.writeAnnotation)(anns)))), Some(hydra.java.serde.writePrimitiveType(pt)))))
}

def writeReceiverParameter[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ReceiverParameter")

def writeReferenceType(rt: hydra.java.syntax.ReferenceType): hydra.ast.Expr =
  rt match
  case hydra.java.syntax.ReferenceType.classOrInterface(v_ReferenceType_classOrInterface_cit) => hydra.java.serde.writeClassOrInterfaceType(v_ReferenceType_classOrInterface_cit)
  case hydra.java.syntax.ReferenceType.variable(v_ReferenceType_variable_v) => hydra.java.serde.writeTypeVariable(v_ReferenceType_variable_v)
  case hydra.java.syntax.ReferenceType.array(v_ReferenceType_array_at) => hydra.java.serde.writeArrayType(v_ReferenceType_array_at)

def writeRelationalExpression(e: hydra.java.syntax.RelationalExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.RelationalExpression.simple(v_RelationalExpression_simple_s) => hydra.java.serde.writeShiftExpression(v_RelationalExpression_simple_s)
  case hydra.java.syntax.RelationalExpression.lessThan(v_RelationalExpression_lessThan_lt) => hydra.java.serde.writeRelationalExpression_LessThan(v_RelationalExpression_lessThan_lt)
  case hydra.java.syntax.RelationalExpression.greaterThan(v_RelationalExpression_greaterThan_gt) => hydra.java.serde.writeRelationalExpression_GreaterThan(v_RelationalExpression_greaterThan_gt)
  case hydra.java.syntax.RelationalExpression.lessThanEqual(v_RelationalExpression_lessThanEqual_lte) => hydra.java.serde.writeRelationalExpression_LessThanEqual(v_RelationalExpression_lessThanEqual_lte)
  case hydra.java.syntax.RelationalExpression.greaterThanEqual(v_RelationalExpression_greaterThanEqual_gte) => hydra.java.serde.writeRelationalExpression_GreaterThanEqual(v_RelationalExpression_greaterThanEqual_gte)
  case hydra.java.syntax.RelationalExpression.instanceof(v_RelationalExpression_instanceof_i) => hydra.java.serde.writeRelationalExpression_InstanceOf(v_RelationalExpression_instanceof_i)

def writeRelationalExpression_GreaterThan(gt: hydra.java.syntax.RelationalExpression_GreaterThan): hydra.ast.Expr =
  hydra.serialization.infixWs(">")(hydra.java.serde.writeRelationalExpression(gt.lhs))(hydra.java.serde.writeShiftExpression(gt.rhs))

def writeRelationalExpression_GreaterThanEqual(gte: hydra.java.syntax.RelationalExpression_GreaterThanEqual): hydra.ast.Expr =
  hydra.serialization.infixWs(">=")(hydra.java.serde.writeRelationalExpression(gte.lhs))(hydra.java.serde.writeShiftExpression(gte.rhs))

def writeRelationalExpression_InstanceOf(io: hydra.java.syntax.RelationalExpression_InstanceOf): hydra.ast.Expr =
  hydra.serialization.infixWs("instanceof")(hydra.java.serde.writeRelationalExpression(io.lhs))(hydra.java.serde.writeReferenceType(io.rhs))

def writeRelationalExpression_LessThan(lt: hydra.java.syntax.RelationalExpression_LessThan): hydra.ast.Expr =
  hydra.serialization.infixWs("<")(hydra.java.serde.writeRelationalExpression(lt.lhs))(hydra.java.serde.writeShiftExpression(lt.rhs))

def writeRelationalExpression_LessThanEqual(lte: hydra.java.syntax.RelationalExpression_LessThanEqual): hydra.ast.Expr =
  hydra.serialization.infixWs("<=")(hydra.java.serde.writeRelationalExpression(lte.lhs))(hydra.java.serde.writeShiftExpression(lte.rhs))

def writeResult(r: hydra.java.syntax.Result): hydra.ast.Expr =
  r match
  case hydra.java.syntax.Result.`type`(v_Result_type_t) => hydra.java.serde.writeUnannType(v_Result_type_t)
  case hydra.java.syntax.Result.void => hydra.serialization.cst("void")

def writeReturnStatement(rs: hydra.java.syntax.ReturnStatement): hydra.ast.Expr =
  {
  lazy val mex: Option[hydra.java.syntax.Expression] = rs
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("return")),
     hydra.lib.maybes.map[hydra.java.syntax.Expression, hydra.ast.Expr](hydra.java.serde.writeExpression)(mex)))))
}

def writeShiftExpression(e: hydra.java.syntax.ShiftExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.ShiftExpression.unary(v_ShiftExpression_unary_a) => hydra.java.serde.writeAdditiveExpression(v_ShiftExpression_unary_a)
  case hydra.java.syntax.ShiftExpression.shiftLeft(v_ShiftExpression_shiftLeft_b) => hydra.serialization.infixWs("<<")(hydra.java.serde.writeShiftExpression(v_ShiftExpression_shiftLeft_b.lhs))(hydra.java.serde.writeAdditiveExpression(v_ShiftExpression_shiftLeft_b.rhs))
  case hydra.java.syntax.ShiftExpression.shiftRight(v_ShiftExpression_shiftRight_b) => hydra.serialization.infixWs(">>")(hydra.java.serde.writeShiftExpression(v_ShiftExpression_shiftRight_b.lhs))(hydra.java.serde.writeAdditiveExpression(v_ShiftExpression_shiftRight_b.rhs))
  case hydra.java.syntax.ShiftExpression.shiftRightZeroFill(v_ShiftExpression_shiftRightZeroFill_b) => hydra.serialization.infixWs(">>>")(hydra.java.serde.writeShiftExpression(v_ShiftExpression_shiftRightZeroFill_b.lhs))(hydra.java.serde.writeAdditiveExpression(v_ShiftExpression_shiftRightZeroFill_b.rhs))

def writeSimpleTypeName(stn: hydra.java.syntax.SimpleTypeName): hydra.ast.Expr = hydra.java.serde.writeTypeIdentifier(stn)

def writeSingleElementAnnotation(sea: hydra.java.syntax.SingleElementAnnotation): hydra.ast.Expr =
  {
  lazy val tname: hydra.java.syntax.TypeName = (sea.name)
  lazy val mv: Option[hydra.java.syntax.ElementValue] = (sea.value)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.java.syntax.ElementValue](hydra.java.serde.writeMarkerAnnotation(tname))((v: hydra.java.syntax.ElementValue) =>
    hydra.serialization.prefix("@")(hydra.serialization.noSep(Seq(hydra.java.serde.writeTypeName(tname),
       hydra.serialization.parenList(false)(Seq(hydra.java.serde.writeElementValue(v)))))))(mv)
}

def writeStatement(s: hydra.java.syntax.Statement): hydra.ast.Expr =
  s match
  case hydra.java.syntax.Statement.withoutTrailing(v_Statement_withoutTrailing_s2) => hydra.java.serde.writeStatementWithoutTrailingSubstatement(v_Statement_withoutTrailing_s2)
  case hydra.java.syntax.Statement.labeled(v_Statement_labeled_l) => hydra.java.serde.writeLabeledStatement(v_Statement_labeled_l)
  case hydra.java.syntax.Statement.ifThen(v_Statement_ifThen_it) => hydra.java.serde.writeIfThenStatement(v_Statement_ifThen_it)
  case hydra.java.syntax.Statement.ifThenElse(v_Statement_ifThenElse_ite) => hydra.java.serde.writeIfThenElseStatement(v_Statement_ifThenElse_ite)
  case hydra.java.syntax.Statement.`while`(v_Statement_while_w) => hydra.java.serde.writeWhileStatement(v_Statement_while_w)
  case hydra.java.syntax.Statement.`for`(v_Statement_for_f) => hydra.java.serde.writeForStatement(v_Statement_for_f)

def writeStatementExpression(e: hydra.java.syntax.StatementExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.StatementExpression.assignment(v_StatementExpression_assignment_a) => hydra.java.serde.writeAssignment(v_StatementExpression_assignment_a)
  case hydra.java.syntax.StatementExpression.preIncrement(v_StatementExpression_preIncrement_pi) => hydra.java.serde.writePreIncrementExpression(v_StatementExpression_preIncrement_pi)
  case hydra.java.syntax.StatementExpression.preDecrement(v_StatementExpression_preDecrement_pd) => hydra.java.serde.writePreDecrementExpression(v_StatementExpression_preDecrement_pd)
  case hydra.java.syntax.StatementExpression.postIncrement(v_StatementExpression_postIncrement_pi) => hydra.java.serde.writePostIncrementExpression(v_StatementExpression_postIncrement_pi)
  case hydra.java.syntax.StatementExpression.postDecrement(v_StatementExpression_postDecrement_pd) => hydra.java.serde.writePostDecrementExpression(v_StatementExpression_postDecrement_pd)
  case hydra.java.syntax.StatementExpression.methodInvocation(v_StatementExpression_methodInvocation_m) => hydra.java.serde.writeMethodInvocation(v_StatementExpression_methodInvocation_m)
  case hydra.java.syntax.StatementExpression.classInstanceCreation(v_StatementExpression_classInstanceCreation_cic) => hydra.java.serde.writeClassInstanceCreationExpression(v_StatementExpression_classInstanceCreation_cic)

def writeStatementWithoutTrailingSubstatement(s: hydra.java.syntax.StatementWithoutTrailingSubstatement): hydra.ast.Expr =
  s match
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.block(v_StatementWithoutTrailingSubstatement_block_b) => hydra.java.serde.writeBlock(v_StatementWithoutTrailingSubstatement_block_b)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.empty => hydra.serialization.cst(";")
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.expression(v_StatementWithoutTrailingSubstatement_expression_e) => hydra.java.serde.writeExpressionStatement(v_StatementWithoutTrailingSubstatement_expression_e)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.assert(v_StatementWithoutTrailingSubstatement_assert_a) => hydra.java.serde.writeAssertStatement(v_StatementWithoutTrailingSubstatement_assert_a)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.switch(v_StatementWithoutTrailingSubstatement_switch_s2) => hydra.java.serde.writeSwitchStatement(v_StatementWithoutTrailingSubstatement_switch_s2)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.`do`(v_StatementWithoutTrailingSubstatement_do_d) => hydra.java.serde.writeDoStatement(v_StatementWithoutTrailingSubstatement_do_d)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.break(v_StatementWithoutTrailingSubstatement_break_b) => hydra.java.serde.writeBreakStatement(v_StatementWithoutTrailingSubstatement_break_b)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.continue(v_StatementWithoutTrailingSubstatement_continue_c) => hydra.java.serde.writeContinueStatement(v_StatementWithoutTrailingSubstatement_continue_c)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.`return`(v_StatementWithoutTrailingSubstatement_return_r) => hydra.java.serde.writeReturnStatement(v_StatementWithoutTrailingSubstatement_return_r)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.synchronized(v_StatementWithoutTrailingSubstatement_synchronized_s2) => hydra.java.serde.writeSynchronizedStatement(v_StatementWithoutTrailingSubstatement_synchronized_s2)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.`throw`(v_StatementWithoutTrailingSubstatement_throw_t) => hydra.java.serde.writeThrowStatement(v_StatementWithoutTrailingSubstatement_throw_t)
  case hydra.java.syntax.StatementWithoutTrailingSubstatement.`try`(v_StatementWithoutTrailingSubstatement_try_t) => hydra.java.serde.writeTryStatement(v_StatementWithoutTrailingSubstatement_try_t)

def writeStaticInitializer[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:StaticInitializer")

def writeStringLiteral(sl: hydra.java.syntax.StringLiteral): hydra.ast.Expr =
  {
  lazy val s: scala.Predef.String = sl
  hydra.serialization.cst(hydra.lib.strings.cat2("\"")(hydra.lib.strings.cat2(hydra.java.serde.escapeJavaString(s))("\"")))
}

def writeSwitchStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:SwitchStatement")

def writeSynchronizedStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:SynchronizedStatement")

def writeThrowStatement(ts: hydra.java.syntax.ThrowStatement): hydra.ast.Expr =
  hydra.serialization.withSemi(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("throw"),
     hydra.java.serde.writeExpression(ts))))

def writeThrows[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:Throws")

def writeTryStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:TryStatement")

def writeType(t: hydra.java.syntax.Type): hydra.ast.Expr =
  t match
  case hydra.java.syntax.Type.primitive(v_Type_primitive_pt) => hydra.java.serde.writePrimitiveTypeWithAnnotations(v_Type_primitive_pt)
  case hydra.java.syntax.Type.reference(v_Type_reference_rt) => hydra.java.serde.writeReferenceType(v_Type_reference_rt)

def writeTypeArgument(a: hydra.java.syntax.TypeArgument): hydra.ast.Expr =
  a match
  case hydra.java.syntax.TypeArgument.reference(v_TypeArgument_reference_rt) => hydra.java.serde.writeReferenceType(v_TypeArgument_reference_rt)
  case hydra.java.syntax.TypeArgument.wildcard(v_TypeArgument_wildcard_w) => hydra.java.serde.writeWildcard(v_TypeArgument_wildcard_w)

def writeTypeArgumentsOrDiamond(targs: hydra.java.syntax.TypeArgumentsOrDiamond): hydra.ast.Expr =
  targs match
  case hydra.java.syntax.TypeArgumentsOrDiamond.arguments(v_TypeArgumentsOrDiamond_arguments_args) => hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.TypeArgument,
     hydra.ast.Expr](hydra.java.serde.writeTypeArgument)(v_TypeArgumentsOrDiamond_arguments_args))
  case hydra.java.syntax.TypeArgumentsOrDiamond.diamond => hydra.serialization.cst("<>")

def writeTypeBound(b: hydra.java.syntax.TypeBound): hydra.ast.Expr =
  b match
  case hydra.java.syntax.TypeBound.variable(v_TypeBound_variable_tv) => hydra.java.serde.writeTypeVariable(v_TypeBound_variable_tv)
  case hydra.java.syntax.TypeBound.classOrInterface(v_TypeBound_classOrInterface_ci) => {
    lazy val cit: hydra.java.syntax.ClassOrInterfaceType = (v_TypeBound_classOrInterface_ci.`type`)
    lazy val additional: Seq[hydra.java.syntax.AdditionalBound] = (v_TypeBound_classOrInterface_ci.additional)
    hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.java.syntax.AdditionalBound](additional))(hydra.java.serde.writeClassOrInterfaceType(cit))(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.java.serde.writeClassOrInterfaceType(cit))(hydra.lib.lists.map[hydra.java.syntax.AdditionalBound,
       hydra.ast.Expr](hydra.java.serde.writeAdditionalBound)(additional))))
  }

def writeTypeDeclaration(d: hydra.java.syntax.TypeDeclaration): hydra.ast.Expr =
  d match
  case hydra.java.syntax.TypeDeclaration.`class`(v_TypeDeclaration_class_d2) => hydra.java.serde.writeClassDeclaration(v_TypeDeclaration_class_d2)
  case hydra.java.syntax.TypeDeclaration.interface(v_TypeDeclaration_interface_d2) => hydra.java.serde.writeInterfaceDeclaration(v_TypeDeclaration_interface_d2)
  case hydra.java.syntax.TypeDeclaration.none => hydra.serialization.cst(";")

def writeTypeDeclarationWithComments(tdwc: hydra.java.syntax.TypeDeclarationWithComments): hydra.ast.Expr =
  {
  lazy val d: hydra.java.syntax.TypeDeclaration = (tdwc.value)
  lazy val mc: Option[scala.Predef.String] = (tdwc.comments)
  hydra.java.serde.withComments(mc)(hydra.java.serde.writeTypeDeclaration(d))
}

def writeTypeIdentifier(tid: hydra.java.syntax.TypeIdentifier): hydra.ast.Expr = hydra.java.serde.writeIdentifier(tid)

def writeTypeName(tn: hydra.java.syntax.TypeName): hydra.ast.Expr =
  {
  lazy val id: hydra.java.syntax.TypeIdentifier = (tn.identifier)
  lazy val mqual: Option[hydra.java.syntax.PackageOrTypeName] = (tn.qualifier)
  hydra.serialization.dotSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.java.syntax.PackageOrTypeName,
     hydra.ast.Expr](hydra.java.serde.writePackageOrTypeName)(mqual), Some(hydra.java.serde.writeTypeIdentifier(id)))))
}

def writeTypeParameter(tp: hydra.java.syntax.TypeParameter): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.java.syntax.TypeParameterModifier] = (tp.modifiers)
  lazy val id: hydra.java.syntax.TypeIdentifier = (tp.identifier)
  lazy val bound: Option[hydra.java.syntax.TypeBound] = (tp.bound)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.TypeParameterModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.TypeParameterModifier,
     hydra.ast.Expr](hydra.java.serde.writeTypeParameterModifier)(mods)))), Some(hydra.java.serde.writeTypeIdentifier(id)),
     hydra.lib.maybes.map[hydra.java.syntax.TypeBound, hydra.ast.Expr]((b: hydra.java.syntax.TypeBound) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("extends"), hydra.java.serde.writeTypeBound(b))))(bound))))
}

def writeTypeParameterModifier(tpm: hydra.java.syntax.TypeParameterModifier): hydra.ast.Expr = hydra.java.serde.writeAnnotation(tpm)

def writeTypeVariable(tv: hydra.java.syntax.TypeVariable): hydra.ast.Expr =
  {
  lazy val anns: Seq[hydra.java.syntax.Annotation] = (tv.annotations)
  lazy val id: hydra.java.syntax.TypeIdentifier = (tv.identifier)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.java.syntax.Annotation,
     hydra.ast.Expr](hydra.java.serde.writeAnnotation)(anns)))), Some(hydra.java.serde.writeTypeIdentifier(id)))))
}

def writeUnannType(ut: hydra.java.syntax.UnannType): hydra.ast.Expr = hydra.java.serde.writeType(ut)

def writeUnaryExpression(e: hydra.java.syntax.UnaryExpression): hydra.ast.Expr =
  e match
  case hydra.java.syntax.UnaryExpression.preIncrement(v_UnaryExpression_preIncrement_pi) => hydra.java.serde.writePreIncrementExpression(v_UnaryExpression_preIncrement_pi)
  case hydra.java.syntax.UnaryExpression.preDecrement(v_UnaryExpression_preDecrement_pd) => hydra.java.serde.writePreDecrementExpression(v_UnaryExpression_preDecrement_pd)
  case hydra.java.syntax.UnaryExpression.plus(v_UnaryExpression_plus_p) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("+"),
     hydra.java.serde.writeUnaryExpression(v_UnaryExpression_plus_p)))
  case hydra.java.syntax.UnaryExpression.minus(v_UnaryExpression_minus_m) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("-"),
     hydra.java.serde.writeUnaryExpression(v_UnaryExpression_minus_m)))
  case hydra.java.syntax.UnaryExpression.other(v_UnaryExpression_other_o) => hydra.java.serde.writeUnaryExpressionNotPlusMinus(v_UnaryExpression_other_o)

def writeUnaryExpressionNotPlusMinus(e: hydra.java.syntax.UnaryExpressionNotPlusMinus): hydra.ast.Expr =
  e match
  case hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(v_UnaryExpressionNotPlusMinus_postfix_p) => hydra.java.serde.writePostfixExpression(v_UnaryExpressionNotPlusMinus_postfix_p)
  case hydra.java.syntax.UnaryExpressionNotPlusMinus.tilde(v_UnaryExpressionNotPlusMinus_tilde_u) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("~"),
     hydra.java.serde.writeUnaryExpression(v_UnaryExpressionNotPlusMinus_tilde_u)))
  case hydra.java.syntax.UnaryExpressionNotPlusMinus.not(v_UnaryExpressionNotPlusMinus_not_u) => hydra.serialization.noSep(Seq(hydra.serialization.cst("!"),
     hydra.java.serde.writeUnaryExpression(v_UnaryExpressionNotPlusMinus_not_u)))
  case hydra.java.syntax.UnaryExpressionNotPlusMinus.cast(v_UnaryExpressionNotPlusMinus_cast_c) => hydra.java.serde.writeCastExpression(v_UnaryExpressionNotPlusMinus_cast_c)

def writeUnqualifiedClassInstanceCreationExpression(ucice: hydra.java.syntax.UnqualifiedClassInstanceCreationExpression): hydra.ast.Expr =
  {
  lazy val targs: Seq[hydra.java.syntax.TypeArgument] = (ucice.typeArguments)
  lazy val cit: hydra.java.syntax.ClassOrInterfaceTypeToInstantiate = (ucice.classOrInterface)
  lazy val args: Seq[hydra.java.syntax.Expression] = (ucice.arguments)
  lazy val mbody: Option[hydra.java.syntax.ClassBody] = (ucice.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("new")),
     hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.TypeArgument](targs))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.TypeArgument,
     hydra.ast.Expr](hydra.java.serde.writeTypeArgument)(targs)))), Some(hydra.serialization.noSep(Seq(hydra.java.serde.writeClassOrInterfaceTypeToInstantiate(cit),
     hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.java.syntax.Expression,
     hydra.ast.Expr](hydra.java.serde.writeExpression)(args))))), hydra.lib.maybes.map[hydra.java.syntax.ClassBody,
     hydra.ast.Expr](hydra.java.serde.writeClassBody)(mbody))))
}

def writeVariableArityParameter[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:VariableArityParameter")

def writeVariableDeclarator(vd: hydra.java.syntax.VariableDeclarator): hydra.ast.Expr =
  {
  lazy val id: hydra.java.syntax.VariableDeclaratorId = (vd.id)
  lazy val minit: Option[hydra.java.syntax.VariableInitializer] = (vd.initializer)
  lazy val idSec: hydra.ast.Expr = hydra.java.serde.writeVariableDeclaratorId(id)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.java.syntax.VariableInitializer](idSec)((init: hydra.java.syntax.VariableInitializer) =>
    hydra.serialization.infixWs("=")(idSec)(hydra.java.serde.writeVariableInitializer(init)))(minit)
}

def writeVariableDeclaratorId(vdi: hydra.java.syntax.VariableDeclaratorId): hydra.ast.Expr =
  {
  lazy val id: hydra.java.syntax.Identifier = (vdi.identifier)
  lazy val mdims: Option[hydra.java.syntax.Dims] = (vdi.dims)
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.java.serde.writeIdentifier(id)),
     hydra.lib.maybes.map[hydra.java.syntax.Dims, hydra.ast.Expr](hydra.java.serde.writeDims)(mdims))))
}

def writeVariableInitializer(i: hydra.java.syntax.VariableInitializer): hydra.ast.Expr =
  i match
  case hydra.java.syntax.VariableInitializer.expression(v_VariableInitializer_expression_e) => hydra.java.serde.writeExpression(v_VariableInitializer_expression_e)
  case hydra.java.syntax.VariableInitializer.arrayInitializer(v_VariableInitializer_arrayInitializer_ai) => hydra.java.serde.writeArrayInitializer(v_VariableInitializer_arrayInitializer_ai)

def writeVariableModifier(m: hydra.java.syntax.VariableModifier): hydra.ast.Expr =
  m match
  case hydra.java.syntax.VariableModifier.annotation(v_VariableModifier_annotation_ann) => hydra.java.serde.writeAnnotation(v_VariableModifier_annotation_ann)
  case hydra.java.syntax.VariableModifier.`final` => hydra.serialization.cst("final")

def writeWhileStatement(ws: hydra.java.syntax.WhileStatement): hydra.ast.Expr =
  {
  lazy val mcond: Option[hydra.java.syntax.Expression] = (ws.cond)
  lazy val body: hydra.java.syntax.Statement = (ws.body)
  lazy val condSer: hydra.ast.Expr = hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.java.syntax.Expression](hydra.serialization.cst("true"))((c: hydra.java.syntax.Expression) => hydra.java.serde.writeExpression(c))(mcond)
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("while"), hydra.serialization.parenList(false)(Seq(condSer)),
     hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.java.serde.writeStatement(body))))
}

def writeWildcard(w: hydra.java.syntax.Wildcard): hydra.ast.Expr =
  {
  lazy val anns: Seq[hydra.java.syntax.Annotation] = (w.annotations)
  lazy val mbounds: Option[hydra.java.syntax.WildcardBounds] = (w.wildcard)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.java.syntax.Annotation,
     hydra.ast.Expr](hydra.java.serde.writeAnnotation)(anns)))), Some(hydra.serialization.cst("*")),
     hydra.lib.maybes.map[hydra.java.syntax.WildcardBounds, hydra.ast.Expr](hydra.java.serde.writeWildcardBounds)(mbounds))))
}

def writeWildcardBounds(b: hydra.java.syntax.WildcardBounds): hydra.ast.Expr =
  b match
  case hydra.java.syntax.WildcardBounds.`extends`(v_WildcardBounds_extends_rt) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("extends"),
     hydra.java.serde.writeReferenceType(v_WildcardBounds_extends_rt)))
  case hydra.java.syntax.WildcardBounds.`super`(v_WildcardBounds_super_rt) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("super"),
     hydra.java.serde.writeReferenceType(v_WildcardBounds_super_rt)))
