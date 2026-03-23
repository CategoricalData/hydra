package hydra.ext.java.serde

import hydra.ext.java.syntax.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.strings

def hexDigit(n: Int): Int =
  hydra.lib.logic.ifElse[Int](hydra.lib.equality.lt[Int](n)(10))(hydra.lib.math.add(n)(48))(hydra.lib.math.add(hydra.lib.math.sub(n)(10))(65))

def padHex4(n: Int): scala.Predef.String =
  {
  lazy val d3: Int = hydra.lib.math.div(n)(4096)
  lazy val r3: Int = hydra.lib.math.mod(n)(4096)
  lazy val d2: Int = hydra.lib.math.div(r3)(256)
  lazy val r2: Int = hydra.lib.math.mod(r3)(256)
  lazy val d1: Int = hydra.lib.math.div(r2)(16)
  lazy val d0: Int = hydra.lib.math.mod(r2)(16)
  hydra.lib.strings.fromList(Seq(hydra.ext.java.serde.hexDigit(d3), hydra.ext.java.serde.hexDigit(d2), hydra.ext.java.serde.hexDigit(d1), hydra.ext.java.serde.hexDigit(d0)))
}

def javaUnicodeEscape(n: Int): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.gt[Int](n)(65535))({
  lazy val `n_`: Int = hydra.lib.math.sub(n)(65536)
  {
    lazy val hi: Int = hydra.lib.math.add(55296)(hydra.lib.math.div(`n_`)(1024))
    {
      lazy val lo: Int = hydra.lib.math.add(56320)(hydra.lib.math.mod(`n_`)(1024))
      hydra.lib.strings.cat2(hydra.lib.strings.cat2("\\u")(hydra.ext.java.serde.padHex4(hi)))(hydra.lib.strings.cat2("\\u")(hydra.ext.java.serde.padHex4(lo)))
    }
  }
})(hydra.lib.strings.cat2("\\u")(hydra.ext.java.serde.padHex4(n)))

def escapeJavaChar(c: Int): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(34))("\\\"")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(92))("\\\\")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(10))("\\n")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(13))("\\r")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(9))("\\t")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(8))("\\b")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](c)(12))("\\f")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.logic.and(hydra.lib.equality.gte[Int](c)(32))(hydra.lib.equality.lt[Int](c)(127)))(hydra.lib.strings.fromList(Seq(c)))(hydra.ext.java.serde.javaUnicodeEscape(c)))))))))

def escapeJavaString(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.cat(hydra.lib.lists.map[Int, scala.Predef.String]((c: Int) => hydra.ext.java.serde.escapeJavaChar(c))(hydra.lib.strings.toList(s)))

def writeAdditionalBound(ab: hydra.ext.java.syntax.AdditionalBound): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("&"), hydra.ext.java.serde.writeInterfaceType(ab)))

def writeAdditiveExpression(e: hydra.ext.java.syntax.AdditiveExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.AdditiveExpression.unary(v_AdditiveExpression_unary_m) => hydra.ext.java.serde.writeMultiplicativeExpression(v_AdditiveExpression_unary_m)
  case hydra.ext.java.syntax.AdditiveExpression.plus(v_AdditiveExpression_plus_b) => hydra.serialization.infixWs("+")(hydra.ext.java.serde.writeAdditiveExpression(v_AdditiveExpression_plus_b.lhs))(hydra.ext.java.serde.writeMultiplicativeExpression(v_AdditiveExpression_plus_b.rhs))
  case hydra.ext.java.syntax.AdditiveExpression.minus(v_AdditiveExpression_minus_b) => hydra.serialization.infixWs("-")(hydra.ext.java.serde.writeAdditiveExpression(v_AdditiveExpression_minus_b.lhs))(hydra.ext.java.serde.writeMultiplicativeExpression(v_AdditiveExpression_minus_b.rhs))

def writeAmbiguousName(an: hydra.ext.java.syntax.AmbiguousName): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.ext.java.syntax.Identifier, hydra.ast.Expr](hydra.ext.java.serde.writeIdentifier)(an))

def writeAndExpression(ae: hydra.ext.java.syntax.AndExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("&")(hydra.lib.lists.map[hydra.ext.java.syntax.EqualityExpression, hydra.ast.Expr](hydra.ext.java.serde.writeEqualityExpression)(ae))

def writeAnnotatedIdentifier(ai: hydra.ext.java.syntax.AnnotatedIdentifier): hydra.ast.Expr = hydra.ext.java.serde.writeIdentifier(ai.identifier)

def writeAnnotation(ann: hydra.ext.java.syntax.Annotation): hydra.ast.Expr =
  ann match
  case hydra.ext.java.syntax.Annotation.normal(v_Annotation_normal_n) => hydra.ext.java.serde.writeNormalAnnotation(v_Annotation_normal_n)
  case hydra.ext.java.syntax.Annotation.marker(v_Annotation_marker_m) => hydra.ext.java.serde.writeMarkerAnnotation(v_Annotation_marker_m)
  case hydra.ext.java.syntax.Annotation.singleElement(v_Annotation_singleElement_s) => hydra.ext.java.serde.writeSingleElementAnnotation(v_Annotation_singleElement_s)

def writeAnnotationTypeDeclaration[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:AnnotationTypeDeclaration")

def writeArrayAccess[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ArrayAccess")

def writeArrayCreationExpression(ace: hydra.ext.java.syntax.ArrayCreationExpression): hydra.ast.Expr =
  ace match
  case hydra.ext.java.syntax.ArrayCreationExpression.primitiveArray(v_ArrayCreationExpression_primitiveArray_pa) => {
    lazy val pt: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations = (v_ArrayCreationExpression_primitiveArray_pa.`type`)
    lazy val ai: hydra.ext.java.syntax.ArrayInitializer = (v_ArrayCreationExpression_primitiveArray_pa.array)
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("new"), hydra.serialization.noSep(Seq(hydra.ext.java.serde.writePrimitiveTypeWithAnnotations(pt), hydra.serialization.cst("[]"))), hydra.ext.java.serde.writeArrayInitializer(ai)))
  }
  case hydra.ext.java.syntax.ArrayCreationExpression.classOrInterfaceArray(v_ArrayCreationExpression_classOrInterfaceArray__) => hydra.serialization.cst("STUB:ArrayCreationExpression")
  case hydra.ext.java.syntax.ArrayCreationExpression.primitive(v_ArrayCreationExpression_primitive__) => hydra.serialization.cst("STUB:ArrayCreationExpression")
  case hydra.ext.java.syntax.ArrayCreationExpression.classOrInterface(v_ArrayCreationExpression_classOrInterface__) => hydra.serialization.cst("STUB:ArrayCreationExpression")

def writeArrayInitializer(ai: hydra.ext.java.syntax.ArrayInitializer): hydra.ast.Expr =
  {
  lazy val groups: Seq[Seq[hydra.ext.java.syntax.VariableInitializer]] = ai
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.equal[Int](hydra.lib.lists.length[Seq[hydra.ext.java.syntax.VariableInitializer]](groups))(1))(hydra.serialization.noSep(Seq(hydra.serialization.cst("{"), hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.VariableInitializer, hydra.ast.Expr](hydra.ext.java.serde.writeVariableInitializer)(hydra.lib.lists.head[Seq[hydra.ext.java.syntax.VariableInitializer]](groups))), hydra.serialization.cst("}"))))(hydra.serialization.cst("{}"))
}

def writeArrayType(at: hydra.ext.java.syntax.ArrayType): hydra.ast.Expr =
  {
  lazy val dims: hydra.ext.java.syntax.Dims = (at.dims)
  lazy val variant: hydra.ext.java.syntax.ArrayType_Variant = (at.variant)
  lazy val varExpr: hydra.ast.Expr = variant match
    case hydra.ext.java.syntax.ArrayType_Variant.primitive(v_ArrayType_Variant_primitive_pt) => hydra.ext.java.serde.writePrimitiveTypeWithAnnotations(v_ArrayType_Variant_primitive_pt)
    case hydra.ext.java.syntax.ArrayType_Variant.classOrInterface(v_ArrayType_Variant_classOrInterface_cit) => hydra.ext.java.serde.writeClassOrInterfaceType(v_ArrayType_Variant_classOrInterface_cit)
    case hydra.ext.java.syntax.ArrayType_Variant.variable(v_ArrayType_Variant_variable_tv) => hydra.ext.java.serde.writeTypeVariable(v_ArrayType_Variant_variable_tv)
  hydra.serialization.noSep(Seq(varExpr, hydra.ext.java.serde.writeDims(dims)))
}

def writeAssertStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:AssertStatement")

def writeAssignment(a: hydra.ext.java.syntax.Assignment): hydra.ast.Expr =
  {
  lazy val lhs: hydra.ext.java.syntax.LeftHandSide = (a.lhs)
  lazy val op: hydra.ext.java.syntax.AssignmentOperator = (a.op)
  lazy val rhs: hydra.ext.java.syntax.Expression = (a.expression)
  lazy val ctop: scala.Predef.String = op match
    case hydra.ext.java.syntax.AssignmentOperator.simple => "="
    case hydra.ext.java.syntax.AssignmentOperator.times => "*="
    case hydra.ext.java.syntax.AssignmentOperator.div => "/="
    case hydra.ext.java.syntax.AssignmentOperator.mod => "%="
    case hydra.ext.java.syntax.AssignmentOperator.plus => "+="
    case hydra.ext.java.syntax.AssignmentOperator.minus => "-="
    case hydra.ext.java.syntax.AssignmentOperator.shiftLeft => "<<="
    case hydra.ext.java.syntax.AssignmentOperator.shiftRight => ">>="
    case hydra.ext.java.syntax.AssignmentOperator.shiftRightZeroFill => ">>>="
    case hydra.ext.java.syntax.AssignmentOperator.and => "&="
    case hydra.ext.java.syntax.AssignmentOperator.xor => "^="
    case hydra.ext.java.syntax.AssignmentOperator.or => "|="
  hydra.serialization.infixWs(ctop)(hydra.ext.java.serde.writeLeftHandSide(lhs))(hydra.ext.java.serde.writeExpression(rhs))
}

def writeAssignmentExpression(e: hydra.ext.java.syntax.AssignmentExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.AssignmentExpression.conditional(v_AssignmentExpression_conditional_c) => hydra.ext.java.serde.writeConditionalExpression(v_AssignmentExpression_conditional_c)
  case hydra.ext.java.syntax.AssignmentExpression.assignment(v_AssignmentExpression_assignment_a) => hydra.ext.java.serde.writeAssignment(v_AssignmentExpression_assignment_a)

def writeBlock(b: hydra.ext.java.syntax.Block): hydra.ast.Expr =
  hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.java.syntax.BlockStatement, hydra.ast.Expr](hydra.ext.java.serde.writeBlockStatement)(b)))

def writeBlockStatement(s: hydra.ext.java.syntax.BlockStatement): hydra.ast.Expr =
  s match
  case hydra.ext.java.syntax.BlockStatement.localVariableDeclaration(v_BlockStatement_localVariableDeclaration_d) => hydra.ext.java.serde.writeLocalVariableDeclarationStatement(v_BlockStatement_localVariableDeclaration_d)
  case hydra.ext.java.syntax.BlockStatement.`class`(v_BlockStatement_class_cd) => hydra.ext.java.serde.writeClassDeclaration(v_BlockStatement_class_cd)
  case hydra.ext.java.syntax.BlockStatement.statement(v_BlockStatement_statement_s2) => hydra.ext.java.serde.writeStatement(v_BlockStatement_statement_s2)

def writeBreakStatement(bs: hydra.ext.java.syntax.BreakStatement): hydra.ast.Expr =
  {
  lazy val mlabel: Option[hydra.ext.java.syntax.Identifier] = bs
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("break")), hydra.lib.maybes.map[hydra.ext.java.syntax.Identifier, hydra.ast.Expr](hydra.ext.java.serde.writeIdentifier)(mlabel)))))
}

def writeCastExpression(e: hydra.ext.java.syntax.CastExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.CastExpression.primitive(v_CastExpression_primitive_p) => hydra.ext.java.serde.writeCastExpression_Primitive(v_CastExpression_primitive_p)
  case hydra.ext.java.syntax.CastExpression.notPlusMinus(v_CastExpression_notPlusMinus_npm) => hydra.ext.java.serde.writeCastExpression_NotPlusMinus(v_CastExpression_notPlusMinus_npm)
  case hydra.ext.java.syntax.CastExpression.lambda(v_CastExpression_lambda_l) => hydra.ext.java.serde.writeCastExpression_Lambda(v_CastExpression_lambda_l)

def writeCastExpression_Lambda[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:CastExpression_Lambda")

def writeCastExpression_NotPlusMinus(npm: hydra.ext.java.syntax.CastExpression_NotPlusMinus): hydra.ast.Expr =
  {
  lazy val rb: hydra.ext.java.syntax.CastExpression_RefAndBounds = (npm.refAndBounds)
  lazy val ex: hydra.ext.java.syntax.UnaryExpression = (npm.expression)
  hydra.serialization.spaceSep(Seq(hydra.ext.java.serde.writeCastExpression_RefAndBounds(rb), hydra.ext.java.serde.writeUnaryExpression(ex)))
}

def writeCastExpression_RefAndBounds(rab: hydra.ext.java.syntax.CastExpression_RefAndBounds): hydra.ast.Expr =
  {
  lazy val rt: hydra.ext.java.syntax.ReferenceType = (rab.`type`)
  lazy val adds: Seq[hydra.ext.java.syntax.AdditionalBound] = (rab.bounds)
  hydra.serialization.parenList(false)(Seq(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.java.serde.writeReferenceType(rt)), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.AdditionalBound](adds))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.AdditionalBound, hydra.ast.Expr](hydra.ext.java.serde.writeAdditionalBound)(adds)))))))))
}

def writeCastExpression_Primitive(cp: hydra.ext.java.syntax.CastExpression_Primitive): hydra.ast.Expr =
  {
  lazy val pt: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations = (cp.`type`)
  lazy val ex: hydra.ext.java.syntax.UnaryExpression = (cp.expression)
  hydra.serialization.spaceSep(Seq(hydra.serialization.parenList(false)(Seq(hydra.ext.java.serde.writePrimitiveTypeWithAnnotations(pt))), hydra.ext.java.serde.writeUnaryExpression(ex)))
}

def writeClassBody(cb: hydra.ext.java.syntax.ClassBody): hydra.ast.Expr =
  hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.serialization.doubleNewlineSep(hydra.lib.lists.map[hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.ast.Expr](hydra.ext.java.serde.writeClassBodyDeclarationWithComments)(cb)))

def writeClassBodyDeclaration(d: hydra.ext.java.syntax.ClassBodyDeclaration): hydra.ast.Expr =
  d match
  case hydra.ext.java.syntax.ClassBodyDeclaration.classMember(v_ClassBodyDeclaration_classMember_d2) => hydra.ext.java.serde.writeClassMemberDeclaration(v_ClassBodyDeclaration_classMember_d2)
  case hydra.ext.java.syntax.ClassBodyDeclaration.instanceInitializer(v_ClassBodyDeclaration_instanceInitializer_i) => hydra.ext.java.serde.writeInstanceInitializer(v_ClassBodyDeclaration_instanceInitializer_i)
  case hydra.ext.java.syntax.ClassBodyDeclaration.staticInitializer(v_ClassBodyDeclaration_staticInitializer_i) => hydra.ext.java.serde.writeStaticInitializer(v_ClassBodyDeclaration_staticInitializer_i)
  case hydra.ext.java.syntax.ClassBodyDeclaration.constructorDeclaration(v_ClassBodyDeclaration_constructorDeclaration_d2) => hydra.ext.java.serde.writeConstructorDeclaration(v_ClassBodyDeclaration_constructorDeclaration_d2)

def writeClassBodyDeclarationWithComments(cbdwc: hydra.ext.java.syntax.ClassBodyDeclarationWithComments): hydra.ast.Expr =
  {
  lazy val d: hydra.ext.java.syntax.ClassBodyDeclaration = (cbdwc.value)
  lazy val mc: Option[scala.Predef.String] = (cbdwc.comments)
  hydra.ext.java.serde.withComments(mc)(hydra.ext.java.serde.writeClassBodyDeclaration(d))
}

def writeClassDeclaration(d: hydra.ext.java.syntax.ClassDeclaration): hydra.ast.Expr =
  d match
  case hydra.ext.java.syntax.ClassDeclaration.normal(v_ClassDeclaration_normal_nd) => hydra.ext.java.serde.writeNormalClassDeclaration(v_ClassDeclaration_normal_nd)
  case hydra.ext.java.syntax.ClassDeclaration.`enum`(v_ClassDeclaration_enum_ed) => hydra.ext.java.serde.writeEnumDeclaration(v_ClassDeclaration_enum_ed)

def writeClassInstanceCreationExpression(cice: hydra.ext.java.syntax.ClassInstanceCreationExpression): hydra.ast.Expr =
  {
  lazy val mqual: Option[hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier] = (cice.qualifier)
  lazy val e: hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression = (cice.expression)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier](hydra.ext.java.serde.writeUnqualifiedClassInstanceCreationExpression(e))((q: hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier) =>
    hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writeClassInstanceCreationExpression_Qualifier(q), hydra.ext.java.serde.writeUnqualifiedClassInstanceCreationExpression(e))))(mqual)
}

def writeClassInstanceCreationExpression_Qualifier(q: hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier): hydra.ast.Expr =
  q match
  case hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier.expression(v_ClassInstanceCreationExpression_Qualifier_expression_en) => hydra.ext.java.serde.writeExpressionName(v_ClassInstanceCreationExpression_Qualifier_expression_en)
  case hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier.primary(v_ClassInstanceCreationExpression_Qualifier_primary_p) => hydra.ext.java.serde.writePrimary(v_ClassInstanceCreationExpression_Qualifier_primary_p)

def writeClassLiteral[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ClassLiteral")

def writeClassMemberDeclaration(d: hydra.ext.java.syntax.ClassMemberDeclaration): hydra.ast.Expr =
  d match
  case hydra.ext.java.syntax.ClassMemberDeclaration.field(v_ClassMemberDeclaration_field_fd) => hydra.ext.java.serde.writeFieldDeclaration(v_ClassMemberDeclaration_field_fd)
  case hydra.ext.java.syntax.ClassMemberDeclaration.method(v_ClassMemberDeclaration_method_md) => hydra.ext.java.serde.writeMethodDeclaration(v_ClassMemberDeclaration_method_md)
  case hydra.ext.java.syntax.ClassMemberDeclaration.`class`(v_ClassMemberDeclaration_class_cd) => hydra.ext.java.serde.writeClassDeclaration(v_ClassMemberDeclaration_class_cd)
  case hydra.ext.java.syntax.ClassMemberDeclaration.interface(v_ClassMemberDeclaration_interface_id) => hydra.ext.java.serde.writeInterfaceDeclaration(v_ClassMemberDeclaration_interface_id)
  case hydra.ext.java.syntax.ClassMemberDeclaration.none => hydra.serialization.cst(";")

def writeClassModifier(m: hydra.ext.java.syntax.ClassModifier): hydra.ast.Expr =
  m match
  case hydra.ext.java.syntax.ClassModifier.annotation(v_ClassModifier_annotation_ann) => hydra.ext.java.serde.writeAnnotation(v_ClassModifier_annotation_ann)
  case hydra.ext.java.syntax.ClassModifier.public => hydra.serialization.cst("public")
  case hydra.ext.java.syntax.ClassModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.ext.java.syntax.ClassModifier.`private` => hydra.serialization.cst("private")
  case hydra.ext.java.syntax.ClassModifier.`abstract` => hydra.serialization.cst("abstract")
  case hydra.ext.java.syntax.ClassModifier.static => hydra.serialization.cst("static")
  case hydra.ext.java.syntax.ClassModifier.`final` => hydra.serialization.cst("final")
  case hydra.ext.java.syntax.ClassModifier.strictfp => hydra.serialization.cst("strictfp")

def writeClassOrInterfaceType(cit: hydra.ext.java.syntax.ClassOrInterfaceType): hydra.ast.Expr =
  cit match
  case hydra.ext.java.syntax.ClassOrInterfaceType.`class`(v_ClassOrInterfaceType_class_ct) => hydra.ext.java.serde.writeClassType(v_ClassOrInterfaceType_class_ct)
  case hydra.ext.java.syntax.ClassOrInterfaceType.interface(v_ClassOrInterfaceType_interface_it) => hydra.ext.java.serde.writeInterfaceType(v_ClassOrInterfaceType_interface_it)

def writeClassOrInterfaceTypeToInstantiate(coitti: hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate): hydra.ast.Expr =
  {
  lazy val ids: Seq[hydra.ext.java.syntax.AnnotatedIdentifier] = (coitti.identifiers)
  lazy val margs: Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond] = (coitti.typeArguments)
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.dotSep(hydra.lib.lists.map[hydra.ext.java.syntax.AnnotatedIdentifier, hydra.ast.Expr](hydra.ext.java.serde.writeAnnotatedIdentifier)(ids))), hydra.lib.maybes.map[hydra.ext.java.syntax.TypeArgumentsOrDiamond, hydra.ast.Expr](hydra.ext.java.serde.writeTypeArgumentsOrDiamond)(margs))))
}

def writeClassType(ct: hydra.ext.java.syntax.ClassType): hydra.ast.Expr =
  {
  lazy val anns: Seq[hydra.ext.java.syntax.Annotation] = (ct.annotations)
  lazy val qual: hydra.ext.java.syntax.ClassTypeQualifier = (ct.qualifier)
  lazy val id: hydra.ext.java.syntax.TypeIdentifier = (ct.identifier)
  lazy val args: Seq[hydra.ext.java.syntax.TypeArgument] = (ct.arguments)
  lazy val qualifiedId: hydra.ast.Expr = qual match
    case hydra.ext.java.syntax.ClassTypeQualifier.none => hydra.ext.java.serde.writeTypeIdentifier(id)
    case hydra.ext.java.syntax.ClassTypeQualifier.`package`(v_ClassTypeQualifier_package_pkg) => hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writePackageName(v_ClassTypeQualifier_package_pkg), hydra.ext.java.serde.writeTypeIdentifier(id)))
    case hydra.ext.java.syntax.ClassTypeQualifier.parent(v_ClassTypeQualifier_parent_cit) => hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writeClassOrInterfaceType(v_ClassTypeQualifier_parent_cit), hydra.ext.java.serde.writeTypeIdentifier(id)))
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.Annotation, hydra.ast.Expr](hydra.ext.java.serde.writeAnnotation)(anns)))), Some(qualifiedId))))), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeArgument](args))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.TypeArgument, hydra.ast.Expr](hydra.ext.java.serde.writeTypeArgument)(args)))))))
}

def writeCompilationUnit(u: hydra.ext.java.syntax.CompilationUnit): hydra.ast.Expr =
  u match
  case hydra.ext.java.syntax.CompilationUnit.ordinary(v_CompilationUnit_ordinary_ocu) => {
    lazy val mpkg: Option[hydra.ext.java.syntax.PackageDeclaration] = (v_CompilationUnit_ordinary_ocu.`package`)
    lazy val imports: Seq[hydra.ext.java.syntax.ImportDeclaration] = (v_CompilationUnit_ordinary_ocu.imports)
    lazy val types: Seq[hydra.ext.java.syntax.TypeDeclarationWithComments] = (v_CompilationUnit_ordinary_ocu.types)
    lazy val warning: Option[hydra.ast.Expr] = Some(hydra.ext.java.serde.singleLineComment(hydra.constants.warningAutoGeneratedFile))
    lazy val pkgSec: Option[hydra.ast.Expr] = hydra.lib.maybes.map[hydra.ext.java.syntax.PackageDeclaration, hydra.ast.Expr](hydra.ext.java.serde.writePackageDeclaration)(mpkg)
    lazy val importsSec: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.ImportDeclaration](imports))(None)(Some(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.java.syntax.ImportDeclaration, hydra.ast.Expr](hydra.ext.java.serde.writeImportDeclaration)(imports))))
    lazy val typesSec: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeDeclarationWithComments](types))(None)(Some(hydra.serialization.doubleNewlineSep(hydra.lib.lists.map[hydra.ext.java.syntax.TypeDeclarationWithComments, hydra.ast.Expr](hydra.ext.java.serde.writeTypeDeclarationWithComments)(types))))
    hydra.serialization.doubleNewlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(warning, pkgSec, importsSec, typesSec)))
  }

def writeConditionalAndExpression(cae: hydra.ext.java.syntax.ConditionalAndExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("&&")(hydra.lib.lists.map[hydra.ext.java.syntax.InclusiveOrExpression, hydra.ast.Expr](hydra.ext.java.serde.writeInclusiveOrExpression)(cae))

def writeConditionalExpression(c: hydra.ext.java.syntax.ConditionalExpression): hydra.ast.Expr =
  c match
  case hydra.ext.java.syntax.ConditionalExpression.simple(v_ConditionalExpression_simple_co) => hydra.ext.java.serde.writeConditionalOrExpression(v_ConditionalExpression_simple_co)
  case hydra.ext.java.syntax.ConditionalExpression.ternaryCond(v_ConditionalExpression_ternaryCond_tc) => hydra.ext.java.serde.writeConditionalExpression_TernaryCond(v_ConditionalExpression_ternaryCond_tc)
  case hydra.ext.java.syntax.ConditionalExpression.ternaryLambda(v_ConditionalExpression_ternaryLambda_tl) => hydra.ext.java.serde.writeConditionalExpression_TernaryLambda(v_ConditionalExpression_ternaryLambda_tl)

def writeConditionalExpression_TernaryCond[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ConditionalExpression_TernaryCond")

def writeConditionalExpression_TernaryLambda[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ConditionalExpression_TernaryLambda")

def writeConditionalOrExpression(coe: hydra.ext.java.syntax.ConditionalOrExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("||")(hydra.lib.lists.map[hydra.ext.java.syntax.ConditionalAndExpression, hydra.ast.Expr](hydra.ext.java.serde.writeConditionalAndExpression)(coe))

def writeConstantDeclaration(cd: hydra.ext.java.syntax.ConstantDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.ConstantModifier] = (cd.modifiers)
  lazy val typ: hydra.ext.java.syntax.UnannType = (cd.`type`)
  lazy val vars: Seq[hydra.ext.java.syntax.VariableDeclarator] = (cd.variables)
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.ConstantModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.ConstantModifier, hydra.ast.Expr](hydra.ext.java.serde.writeConstantModifier)(mods)))), Some(hydra.ext.java.serde.writeUnannType(typ)), Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.VariableDeclarator, hydra.ast.Expr](hydra.ext.java.serde.writeVariableDeclarator)(vars)))))))
}

def writeConstantModifier[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ConstantModifier")

def writeConstructorBody(cb: hydra.ext.java.syntax.ConstructorBody): hydra.ast.Expr =
  {
  lazy val minvoc: Option[hydra.ext.java.syntax.ExplicitConstructorInvocation] = (cb.invocation)
  lazy val stmts: Seq[hydra.ext.java.syntax.BlockStatement] = (cb.statements)
  hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.serialization.doubleNewlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.ext.java.syntax.ExplicitConstructorInvocation, hydra.ast.Expr](hydra.ext.java.serde.writeExplicitConstructorInvocation)(minvoc), Some(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.java.syntax.BlockStatement, hydra.ast.Expr](hydra.ext.java.serde.writeBlockStatement)(stmts)))))))
}

def writeConstructorDeclaration(cd: hydra.ext.java.syntax.ConstructorDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.ConstructorModifier] = (cd.modifiers)
  lazy val cons: hydra.ext.java.syntax.ConstructorDeclarator = (cd.constructor)
  lazy val mthrows: Option[hydra.ext.java.syntax.Throws] = (cd.throws)
  lazy val body: hydra.ext.java.syntax.ConstructorBody = (cd.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.ConstructorModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.ConstructorModifier, hydra.ast.Expr](hydra.ext.java.serde.writeConstructorModifier)(mods)))), Some(hydra.ext.java.serde.writeConstructorDeclarator(cons)), hydra.lib.maybes.map[hydra.ext.java.syntax.Throws, hydra.ast.Expr](hydra.ext.java.serde.writeThrows)(mthrows), Some(hydra.ext.java.serde.writeConstructorBody(body)))))
}

def writeConstructorDeclarator(cd: hydra.ext.java.syntax.ConstructorDeclarator): hydra.ast.Expr =
  {
  lazy val tparams: Seq[hydra.ext.java.syntax.TypeParameter] = (cd.parameters)
  lazy val name: hydra.ext.java.syntax.SimpleTypeName = (cd.name)
  lazy val fparams: Seq[hydra.ext.java.syntax.FormalParameter] = (cd.formalParameters)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter, hydra.ast.Expr](hydra.ext.java.serde.writeTypeParameter)(tparams)))), Some(hydra.ext.java.serde.writeSimpleTypeName(name)), Some(hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.java.syntax.FormalParameter, hydra.ast.Expr](hydra.ext.java.serde.writeFormalParameter)(fparams))))))
}

def writeConstructorModifier(m: hydra.ext.java.syntax.ConstructorModifier): hydra.ast.Expr =
  m match
  case hydra.ext.java.syntax.ConstructorModifier.annotation(v_ConstructorModifier_annotation_ann) => hydra.ext.java.serde.writeAnnotation(v_ConstructorModifier_annotation_ann)
  case hydra.ext.java.syntax.ConstructorModifier.public => hydra.serialization.cst("public")
  case hydra.ext.java.syntax.ConstructorModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.ext.java.syntax.ConstructorModifier.`private` => hydra.serialization.cst("private")

def writeContinueStatement(cs: hydra.ext.java.syntax.ContinueStatement): hydra.ast.Expr =
  {
  lazy val mlabel: Option[hydra.ext.java.syntax.Identifier] = cs
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("continue")), hydra.lib.maybes.map[hydra.ext.java.syntax.Identifier, hydra.ast.Expr](hydra.ext.java.serde.writeIdentifier)(mlabel)))))
}

def writeDims(d: hydra.ext.java.syntax.Dims): hydra.ast.Expr =
  hydra.serialization.noSep(hydra.lib.lists.map[Seq[hydra.ext.java.syntax.Annotation], hydra.ast.Expr]((_x: Seq[hydra.ext.java.syntax.Annotation]) => hydra.serialization.cst("[]"))(d))

def writeDoStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:DoStatement")

def writeElementValue(ev: hydra.ext.java.syntax.ElementValue): hydra.ast.Expr =
  ev match
  case hydra.ext.java.syntax.ElementValue.conditionalExpression(v_ElementValue_conditionalExpression_c) => hydra.ext.java.serde.writeConditionalExpression(v_ElementValue_conditionalExpression_c)
  case hydra.ext.java.syntax.ElementValue.elementValueArrayInitializer(v_ElementValue_elementValueArrayInitializer_evai) => hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.ElementValue, hydra.ast.Expr](hydra.ext.java.serde.writeElementValue)(v_ElementValue_elementValueArrayInitializer_evai))
  case hydra.ext.java.syntax.ElementValue.annotation(v_ElementValue_annotation_ann) => hydra.ext.java.serde.writeAnnotation(v_ElementValue_annotation_ann)

def writeElementValuePair(evp: hydra.ext.java.syntax.ElementValuePair): hydra.ast.Expr =
  {
  lazy val k: hydra.ext.java.syntax.Identifier = (evp.key)
  lazy val v: hydra.ext.java.syntax.ElementValue = (evp.value)
  hydra.serialization.infixWs("=")(hydra.ext.java.serde.writeIdentifier(k))(hydra.ext.java.serde.writeElementValue(v))
}

def writeEnumDeclaration[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:EnumDeclaration")

def writeEqualityExpression(e: hydra.ext.java.syntax.EqualityExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.EqualityExpression.unary(v_EqualityExpression_unary_r) => hydra.ext.java.serde.writeRelationalExpression(v_EqualityExpression_unary_r)
  case hydra.ext.java.syntax.EqualityExpression.equal(v_EqualityExpression_equal_b) => hydra.serialization.infixWs("==")(hydra.ext.java.serde.writeEqualityExpression(v_EqualityExpression_equal_b.lhs))(hydra.ext.java.serde.writeRelationalExpression(v_EqualityExpression_equal_b.rhs))
  case hydra.ext.java.syntax.EqualityExpression.notEqual(v_EqualityExpression_notEqual_b) => hydra.serialization.infixWs("!=")(hydra.ext.java.serde.writeEqualityExpression(v_EqualityExpression_notEqual_b.lhs))(hydra.ext.java.serde.writeRelationalExpression(v_EqualityExpression_notEqual_b.rhs))

def writeExclusiveOrExpression(eoe: hydra.ext.java.syntax.ExclusiveOrExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("^")(hydra.lib.lists.map[hydra.ext.java.syntax.AndExpression, hydra.ast.Expr](hydra.ext.java.serde.writeAndExpression)(eoe))

def writeExplicitConstructorInvocation[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ExplicitConstructorInvocation")

def writeExpression(e: hydra.ext.java.syntax.Expression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.Expression.lambda(v_Expression_lambda_l) => hydra.ext.java.serde.writeLambdaExpression(v_Expression_lambda_l)
  case hydra.ext.java.syntax.Expression.assignment(v_Expression_assignment_a) => hydra.ext.java.serde.writeAssignmentExpression(v_Expression_assignment_a)

def writeExpressionName(en: hydra.ext.java.syntax.ExpressionName): hydra.ast.Expr =
  {
  lazy val mqual: Option[hydra.ext.java.syntax.AmbiguousName] = (en.qualifier)
  lazy val id: hydra.ext.java.syntax.Identifier = (en.identifier)
  hydra.serialization.dotSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.ext.java.syntax.AmbiguousName, hydra.ast.Expr](hydra.ext.java.serde.writeAmbiguousName)(mqual), Some(hydra.ext.java.serde.writeIdentifier(id)))))
}

def writeExpressionStatement(es: hydra.ext.java.syntax.ExpressionStatement): hydra.ast.Expr = hydra.serialization.withSemi(hydra.ext.java.serde.writeStatementExpression(es))

def writeFieldAccess(fa: hydra.ext.java.syntax.FieldAccess): hydra.ast.Expr =
  {
  lazy val qual: hydra.ext.java.syntax.FieldAccess_Qualifier = (fa.qualifier)
  lazy val id: hydra.ext.java.syntax.Identifier = (fa.identifier)
  qual match
    case hydra.ext.java.syntax.FieldAccess_Qualifier.primary(v_FieldAccess_Qualifier_primary_p) => hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writePrimary(v_FieldAccess_Qualifier_primary_p), hydra.ext.java.serde.writeIdentifier(id)))
    case hydra.ext.java.syntax.FieldAccess_Qualifier.`super` => hydra.serialization.dotSep(Seq(hydra.serialization.cst("super"), hydra.ext.java.serde.writeIdentifier(id)))
    case hydra.ext.java.syntax.FieldAccess_Qualifier.typed(v_FieldAccess_Qualifier_typed_tn) => hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writeTypeName(v_FieldAccess_Qualifier_typed_tn), hydra.serialization.cst("super"), hydra.ext.java.serde.writeIdentifier(id)))
}

def writeFieldDeclaration(fd: hydra.ext.java.syntax.FieldDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.FieldModifier] = (fd.modifiers)
  lazy val typ: hydra.ext.java.syntax.UnannType = (fd.unannType)
  lazy val vars: Seq[hydra.ext.java.syntax.VariableDeclarator] = (fd.variableDeclarators)
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.FieldModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.FieldModifier, hydra.ast.Expr](hydra.ext.java.serde.writeFieldModifier)(mods)))), Some(hydra.ext.java.serde.writeUnannType(typ)), Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.VariableDeclarator, hydra.ast.Expr](hydra.ext.java.serde.writeVariableDeclarator)(vars)))))))
}

def writeFieldModifier(m: hydra.ext.java.syntax.FieldModifier): hydra.ast.Expr =
  m match
  case hydra.ext.java.syntax.FieldModifier.annotation(v_FieldModifier_annotation_ann) => hydra.ext.java.serde.writeAnnotation(v_FieldModifier_annotation_ann)
  case hydra.ext.java.syntax.FieldModifier.public => hydra.serialization.cst("public")
  case hydra.ext.java.syntax.FieldModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.ext.java.syntax.FieldModifier.`private` => hydra.serialization.cst("private")
  case hydra.ext.java.syntax.FieldModifier.static => hydra.serialization.cst("static")
  case hydra.ext.java.syntax.FieldModifier.`final` => hydra.serialization.cst("final")
  case hydra.ext.java.syntax.FieldModifier.transient => hydra.serialization.cst("transient")
  case hydra.ext.java.syntax.FieldModifier.volatile => hydra.serialization.cst("volatile")

def writeFloatingPointLiteral(fl: hydra.ext.java.syntax.FloatingPointLiteral): hydra.ast.Expr = hydra.serialization.cst(hydra.lib.literals.showBigfloat(fl))

def writeFloatingPointType(ft: hydra.ext.java.syntax.FloatingPointType): hydra.ast.Expr =
  ft match
  case hydra.ext.java.syntax.FloatingPointType.float => hydra.serialization.cst("float")
  case hydra.ext.java.syntax.FloatingPointType.double => hydra.serialization.cst("double")

def writeForStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ForStatement")

def writeFormalParameter(p: hydra.ext.java.syntax.FormalParameter): hydra.ast.Expr =
  p match
  case hydra.ext.java.syntax.FormalParameter.simple(v_FormalParameter_simple_s) => hydra.ext.java.serde.writeFormalParameter_Simple(v_FormalParameter_simple_s)
  case hydra.ext.java.syntax.FormalParameter.variableArity(v_FormalParameter_variableArity_v) => hydra.ext.java.serde.writeVariableArityParameter(v_FormalParameter_variableArity_v)

def writeFormalParameter_Simple(fps: hydra.ext.java.syntax.FormalParameter_Simple): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.VariableModifier] = (fps.modifiers)
  lazy val typ: hydra.ext.java.syntax.UnannType = (fps.`type`)
  lazy val id: hydra.ext.java.syntax.VariableDeclaratorId = (fps.id)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.VariableModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.VariableModifier, hydra.ast.Expr](hydra.ext.java.serde.writeVariableModifier)(mods)))), Some(hydra.ext.java.serde.writeUnannType(typ)), Some(hydra.ext.java.serde.writeVariableDeclaratorId(id)))))
}

def writeIdentifier(id: hydra.ext.java.syntax.Identifier): hydra.ast.Expr = hydra.serialization.cst(id)

def writeIfThenStatement(its: hydra.ext.java.syntax.IfThenStatement): hydra.ast.Expr =
  {
  lazy val cond: hydra.ext.java.syntax.Expression = (its.expression)
  lazy val thn: hydra.ext.java.syntax.Statement = (its.statement)
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("if"), hydra.serialization.parenList(false)(Seq(hydra.ext.java.serde.writeExpression(cond))), hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.ext.java.serde.writeStatement(thn))))
}

def writeIfThenElseStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:IfThenElseStatement")

def writeImportDeclaration(imp: hydra.ext.java.syntax.ImportDeclaration): hydra.ast.Expr =
  imp match
  case hydra.ext.java.syntax.ImportDeclaration.singleType(v_ImportDeclaration_singleType_st) => hydra.serialization.withSemi(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("import"), hydra.ext.java.serde.writeTypeName(v_ImportDeclaration_singleType_st))))
  case hydra.ext.java.syntax.ImportDeclaration.typeImportOnDemand(v_ImportDeclaration_typeImportOnDemand__) => hydra.serialization.cst("STUB:ImportDeclarationTypeImportOnDemand")
  case hydra.ext.java.syntax.ImportDeclaration.singleStaticImport(v_ImportDeclaration_singleStaticImport__) => hydra.serialization.cst("STUB:ImportDeclarationSingleStaticImport")
  case hydra.ext.java.syntax.ImportDeclaration.staticImportOnDemand(v_ImportDeclaration_staticImportOnDemand__) => hydra.serialization.cst("STUB:ImportDeclarationStaticImportOnDemand")

def writeInclusiveOrExpression(ioe: hydra.ext.java.syntax.InclusiveOrExpression): hydra.ast.Expr =
  hydra.serialization.infixWsList("|")(hydra.lib.lists.map[hydra.ext.java.syntax.ExclusiveOrExpression, hydra.ast.Expr](hydra.ext.java.serde.writeExclusiveOrExpression)(ioe))

def writeInstanceInitializer[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:InstanceInitializer")

def writeIntegerLiteral(il: hydra.ext.java.syntax.IntegerLiteral): hydra.ast.Expr =
  {
  lazy val i: BigInt = il
  lazy val suffix: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.logic.or(hydra.lib.equality.gt[BigInt](i)(BigInt(2147483647L)))(hydra.lib.equality.lt[BigInt](i)(BigInt(-2147483648L))))("L")("")
  hydra.serialization.cst(hydra.lib.strings.cat2(hydra.lib.literals.showBigint(i))(suffix))
}

def writeIntegralType(t: hydra.ext.java.syntax.IntegralType): hydra.ast.Expr =
  t match
  case hydra.ext.java.syntax.IntegralType.byte => hydra.serialization.cst("byte")
  case hydra.ext.java.syntax.IntegralType.short => hydra.serialization.cst("short")
  case hydra.ext.java.syntax.IntegralType.int => hydra.serialization.cst("int")
  case hydra.ext.java.syntax.IntegralType.long => hydra.serialization.cst("long")
  case hydra.ext.java.syntax.IntegralType.char => hydra.serialization.cst("char")

def writeInterfaceBody(ib: hydra.ext.java.syntax.InterfaceBody): hydra.ast.Expr =
  hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.serialization.doubleNewlineSep(hydra.lib.lists.map[hydra.ext.java.syntax.InterfaceMemberDeclaration, hydra.ast.Expr](hydra.ext.java.serde.writeInterfaceMemberDeclaration)(ib)))

def writeInterfaceDeclaration(d: hydra.ext.java.syntax.InterfaceDeclaration): hydra.ast.Expr =
  d match
  case hydra.ext.java.syntax.InterfaceDeclaration.normalInterface(v_InterfaceDeclaration_normalInterface_n) => hydra.ext.java.serde.writeNormalInterfaceDeclaration(v_InterfaceDeclaration_normalInterface_n)
  case hydra.ext.java.syntax.InterfaceDeclaration.annotationType(v_InterfaceDeclaration_annotationType_a) => hydra.ext.java.serde.writeAnnotationTypeDeclaration(v_InterfaceDeclaration_annotationType_a)

def writeInterfaceMemberDeclaration(d: hydra.ext.java.syntax.InterfaceMemberDeclaration): hydra.ast.Expr =
  d match
  case hydra.ext.java.syntax.InterfaceMemberDeclaration.constant(v_InterfaceMemberDeclaration_constant_c) => hydra.ext.java.serde.writeConstantDeclaration(v_InterfaceMemberDeclaration_constant_c)
  case hydra.ext.java.syntax.InterfaceMemberDeclaration.interfaceMethod(v_InterfaceMemberDeclaration_interfaceMethod_im) => hydra.ext.java.serde.writeInterfaceMethodDeclaration(v_InterfaceMemberDeclaration_interfaceMethod_im)
  case hydra.ext.java.syntax.InterfaceMemberDeclaration.`class`(v_InterfaceMemberDeclaration_class_cd) => hydra.ext.java.serde.writeClassDeclaration(v_InterfaceMemberDeclaration_class_cd)
  case hydra.ext.java.syntax.InterfaceMemberDeclaration.interface(v_InterfaceMemberDeclaration_interface_id) => hydra.ext.java.serde.writeInterfaceDeclaration(v_InterfaceMemberDeclaration_interface_id)

def writeInterfaceMethodDeclaration(imd: hydra.ext.java.syntax.InterfaceMethodDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.InterfaceMethodModifier] = (imd.modifiers)
  lazy val header: hydra.ext.java.syntax.MethodHeader = (imd.header)
  lazy val body: hydra.ext.java.syntax.MethodBody = (imd.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.InterfaceMethodModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.InterfaceMethodModifier, hydra.ast.Expr](hydra.ext.java.serde.writeInterfaceMethodModifier)(mods)))), Some(hydra.ext.java.serde.writeMethodHeader(header)), Some(hydra.ext.java.serde.writeMethodBody(body)))))
}

def writeInterfaceMethodModifier(m: hydra.ext.java.syntax.InterfaceMethodModifier): hydra.ast.Expr =
  m match
  case hydra.ext.java.syntax.InterfaceMethodModifier.annotation(v_InterfaceMethodModifier_annotation_a) => hydra.ext.java.serde.writeAnnotation(v_InterfaceMethodModifier_annotation_a)
  case hydra.ext.java.syntax.InterfaceMethodModifier.public => hydra.serialization.cst("public")
  case hydra.ext.java.syntax.InterfaceMethodModifier.`private` => hydra.serialization.cst("private")
  case hydra.ext.java.syntax.InterfaceMethodModifier.`abstract` => hydra.serialization.cst("abstract")
  case hydra.ext.java.syntax.InterfaceMethodModifier.default => hydra.serialization.cst("default")
  case hydra.ext.java.syntax.InterfaceMethodModifier.static => hydra.serialization.cst("static")
  case hydra.ext.java.syntax.InterfaceMethodModifier.strictfp => hydra.serialization.cst("strictfp")

def writeInterfaceModifier(m: hydra.ext.java.syntax.InterfaceModifier): hydra.ast.Expr =
  m match
  case hydra.ext.java.syntax.InterfaceModifier.annotation(v_InterfaceModifier_annotation_a) => hydra.ext.java.serde.writeAnnotation(v_InterfaceModifier_annotation_a)
  case hydra.ext.java.syntax.InterfaceModifier.public => hydra.serialization.cst("public")
  case hydra.ext.java.syntax.InterfaceModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.ext.java.syntax.InterfaceModifier.`private` => hydra.serialization.cst("private")
  case hydra.ext.java.syntax.InterfaceModifier.`abstract` => hydra.serialization.cst("abstract")
  case hydra.ext.java.syntax.InterfaceModifier.static => hydra.serialization.cst("static")
  case hydra.ext.java.syntax.InterfaceModifier.strictfb => hydra.serialization.cst("strictfb")

def writeInterfaceType(it: hydra.ext.java.syntax.InterfaceType): hydra.ast.Expr = hydra.ext.java.serde.writeClassType(it)

def writeLabeledStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:LabeledStatement")

def writeLambdaBody(b: hydra.ext.java.syntax.LambdaBody): hydra.ast.Expr =
  b match
  case hydra.ext.java.syntax.LambdaBody.expression(v_LambdaBody_expression_e) => hydra.ext.java.serde.writeExpression(v_LambdaBody_expression_e)
  case hydra.ext.java.syntax.LambdaBody.block(v_LambdaBody_block_b2) => hydra.ext.java.serde.writeBlock(v_LambdaBody_block_b2)

def writeLambdaExpression(le: hydra.ext.java.syntax.LambdaExpression): hydra.ast.Expr =
  {
  lazy val params: hydra.ext.java.syntax.LambdaParameters = (le.parameters)
  lazy val body: hydra.ext.java.syntax.LambdaBody = (le.body)
  hydra.serialization.infixWs("->")(hydra.ext.java.serde.writeLambdaParameters(params))(hydra.ext.java.serde.writeLambdaBody(body))
}

def writeLambdaParameters(p: hydra.ext.java.syntax.LambdaParameters): hydra.ast.Expr =
  p match
  case hydra.ext.java.syntax.LambdaParameters.tuple(v_LambdaParameters_tuple_l) => hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.java.syntax.LambdaParameters, hydra.ast.Expr](hydra.ext.java.serde.writeLambdaParameters)(v_LambdaParameters_tuple_l))
  case hydra.ext.java.syntax.LambdaParameters.single(v_LambdaParameters_single_id) => hydra.ext.java.serde.writeIdentifier(v_LambdaParameters_single_id)

def writeLeftHandSide(lhs: hydra.ext.java.syntax.LeftHandSide): hydra.ast.Expr =
  lhs match
  case hydra.ext.java.syntax.LeftHandSide.expressionName(v_LeftHandSide_expressionName_en) => hydra.ext.java.serde.writeExpressionName(v_LeftHandSide_expressionName_en)
  case hydra.ext.java.syntax.LeftHandSide.fieldAccess(v_LeftHandSide_fieldAccess_fa) => hydra.ext.java.serde.writeFieldAccess(v_LeftHandSide_fieldAccess_fa)
  case hydra.ext.java.syntax.LeftHandSide.arrayAccess(v_LeftHandSide_arrayAccess_aa) => hydra.ext.java.serde.writeArrayAccess(v_LeftHandSide_arrayAccess_aa)

def writeLiteral(l: hydra.ext.java.syntax.Literal): hydra.ast.Expr =
  l match
  case hydra.ext.java.syntax.Literal.`null` => hydra.serialization.cst("null")
  case hydra.ext.java.syntax.Literal.integer(v_Literal_integer_il) => hydra.ext.java.serde.writeIntegerLiteral(v_Literal_integer_il)
  case hydra.ext.java.syntax.Literal.floatingPoint(v_Literal_floatingPoint_fl) => hydra.ext.java.serde.writeFloatingPointLiteral(v_Literal_floatingPoint_fl)
  case hydra.ext.java.syntax.Literal.boolean(v_Literal_boolean_b) => hydra.serialization.cst(hydra.lib.logic.ifElse[scala.Predef.String](v_Literal_boolean_b)("true")("false"))
  case hydra.ext.java.syntax.Literal.character(v_Literal_character_c) => {
    lazy val ci: Int = hydra.lib.literals.bigintToInt32(hydra.lib.literals.uint16ToBigint(v_Literal_character_c))
    hydra.serialization.cst(hydra.lib.strings.cat2("'")(hydra.lib.strings.cat2(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(39))("\\'")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(92))("\\\\")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(10))("\\n")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(13))("\\r")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](ci)(9))("\\t")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.logic.and(hydra.lib.equality.gte[Int](ci)(32))(hydra.lib.equality.lt[Int](ci)(127)))(hydra.lib.strings.fromList(Seq(ci)))(hydra.ext.java.serde.javaUnicodeEscape(ci))))))))("'")))
  }
  case hydra.ext.java.syntax.Literal.string(v_Literal_string_sl) => hydra.ext.java.serde.writeStringLiteral(v_Literal_string_sl)

def writeLocalVariableDeclaration(lvd: hydra.ext.java.syntax.LocalVariableDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.VariableModifier] = (lvd.modifiers)
  lazy val t: hydra.ext.java.syntax.LocalVariableType = (lvd.`type`)
  lazy val decls: Seq[hydra.ext.java.syntax.VariableDeclarator] = (lvd.declarators)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.VariableModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.VariableModifier, hydra.ast.Expr](hydra.ext.java.serde.writeVariableModifier)(mods)))), Some(hydra.ext.java.serde.writeLocalName(t)), Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.VariableDeclarator, hydra.ast.Expr](hydra.ext.java.serde.writeVariableDeclarator)(decls))))))
}

def writeLocalVariableDeclarationStatement(lvds: hydra.ext.java.syntax.LocalVariableDeclarationStatement): hydra.ast.Expr =
  hydra.serialization.withSemi(hydra.ext.java.serde.writeLocalVariableDeclaration(lvds))

def writeLocalName(t: hydra.ext.java.syntax.LocalVariableType): hydra.ast.Expr =
  t match
  case hydra.ext.java.syntax.LocalVariableType.`type`(v_LocalVariableType_type_ut) => hydra.ext.java.serde.writeUnannType(v_LocalVariableType_type_ut)
  case hydra.ext.java.syntax.LocalVariableType.`var` => hydra.serialization.cst("var")

def writeMarkerAnnotation(ma: hydra.ext.java.syntax.MarkerAnnotation): hydra.ast.Expr = hydra.serialization.prefix("@")(hydra.ext.java.serde.writeTypeName(ma))

def writeMethodBody(b: hydra.ext.java.syntax.MethodBody): hydra.ast.Expr =
  b match
  case hydra.ext.java.syntax.MethodBody.block(v_MethodBody_block_block) => hydra.ext.java.serde.writeBlock(v_MethodBody_block_block)
  case hydra.ext.java.syntax.MethodBody.none => hydra.serialization.cst(";")

def writeMethodDeclaration(md: hydra.ext.java.syntax.MethodDeclaration): hydra.ast.Expr =
  {
  lazy val anns: Seq[hydra.ext.java.syntax.Annotation] = (md.annotations)
  lazy val mods: Seq[hydra.ext.java.syntax.MethodModifier] = (md.modifiers)
  lazy val header: hydra.ext.java.syntax.MethodHeader = (md.header)
  lazy val body: hydra.ext.java.syntax.MethodBody = (md.body)
  lazy val headerAndBody: hydra.ast.Expr = hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.MethodModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.MethodModifier, hydra.ast.Expr](hydra.ext.java.serde.writeMethodModifier)(mods)))), Some(hydra.ext.java.serde.writeMethodHeader(header)), Some(hydra.ext.java.serde.writeMethodBody(body)))))
  hydra.serialization.newlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.java.syntax.Annotation, hydra.ast.Expr](hydra.ext.java.serde.writeAnnotation)(anns)))), Some(headerAndBody))))
}

def writeMethodDeclarator(md: hydra.ext.java.syntax.MethodDeclarator): hydra.ast.Expr =
  {
  lazy val id: hydra.ext.java.syntax.Identifier = (md.identifier)
  lazy val params: Seq[hydra.ext.java.syntax.FormalParameter] = (md.formalParameters)
  hydra.serialization.noSep(Seq(hydra.ext.java.serde.writeIdentifier(id), hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.java.syntax.FormalParameter, hydra.ast.Expr](hydra.ext.java.serde.writeFormalParameter)(params))))
}

def writeMethodHeader(mh: hydra.ext.java.syntax.MethodHeader): hydra.ast.Expr =
  {
  lazy val params: Seq[hydra.ext.java.syntax.TypeParameter] = (mh.parameters)
  lazy val result: hydra.ext.java.syntax.Result = (mh.result)
  lazy val decl: hydra.ext.java.syntax.MethodDeclarator = (mh.declarator)
  lazy val mthrows: Option[hydra.ext.java.syntax.Throws] = (mh.throws)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeParameter](params))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter, hydra.ast.Expr](hydra.ext.java.serde.writeTypeParameter)(params)))), Some(hydra.ext.java.serde.writeResult(result)), Some(hydra.ext.java.serde.writeMethodDeclarator(decl)), hydra.lib.maybes.map[hydra.ext.java.syntax.Throws, hydra.ast.Expr](hydra.ext.java.serde.writeThrows)(mthrows))))
}

def writeMethodInvocation(mi: hydra.ext.java.syntax.MethodInvocation): hydra.ast.Expr =
  {
  lazy val header: hydra.ext.java.syntax.MethodInvocation_Header = (mi.header)
  lazy val args: Seq[hydra.ext.java.syntax.Expression] = (mi.arguments)
  lazy val argSec: hydra.ast.Expr = hydra.serialization.parenList(true)(hydra.lib.lists.map[hydra.ext.java.syntax.Expression, hydra.ast.Expr](hydra.ext.java.serde.writeExpression)(args))
  lazy val headerSec: hydra.ast.Expr = header match
    case hydra.ext.java.syntax.MethodInvocation_Header.simple(v_MethodInvocation_Header_simple_mname) => hydra.ext.java.serde.writeMethodName(v_MethodInvocation_Header_simple_mname)
    case hydra.ext.java.syntax.MethodInvocation_Header.complex(v_MethodInvocation_Header_complex_cx) => {
      lazy val cvar: hydra.ext.java.syntax.MethodInvocation_Variant = (v_MethodInvocation_Header_complex_cx.variant)
      lazy val targs: Seq[hydra.ext.java.syntax.TypeArgument] = (v_MethodInvocation_Header_complex_cx.typeArguments)
      lazy val cid: hydra.ext.java.syntax.Identifier = (v_MethodInvocation_Header_complex_cx.identifier)
      lazy val idSec: hydra.ast.Expr = hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeArgument](targs))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.TypeArgument, hydra.ast.Expr](hydra.ext.java.serde.writeTypeArgument)(targs)))), Some(hydra.ext.java.serde.writeIdentifier(cid)))))
      cvar match
        case hydra.ext.java.syntax.MethodInvocation_Variant.`type`(v_MethodInvocation_Variant_type_tname) => hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writeTypeName(v_MethodInvocation_Variant_type_tname), idSec))
        case hydra.ext.java.syntax.MethodInvocation_Variant.expression(v_MethodInvocation_Variant_expression_en) => hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writeExpressionName(v_MethodInvocation_Variant_expression_en), idSec))
        case hydra.ext.java.syntax.MethodInvocation_Variant.primary(v_MethodInvocation_Variant_primary_p) => hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writePrimary(v_MethodInvocation_Variant_primary_p), idSec))
        case hydra.ext.java.syntax.MethodInvocation_Variant.`super` => hydra.serialization.dotSep(Seq(hydra.serialization.cst("super"), idSec))
        case hydra.ext.java.syntax.MethodInvocation_Variant.typeSuper(v_MethodInvocation_Variant_typeSuper_tname) => hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writeTypeName(v_MethodInvocation_Variant_typeSuper_tname), hydra.serialization.cst("super"), idSec))
    }
  hydra.serialization.noSep(Seq(headerSec, argSec))
}

def writeMethodModifier(m: hydra.ext.java.syntax.MethodModifier): hydra.ast.Expr =
  m match
  case hydra.ext.java.syntax.MethodModifier.annotation(v_MethodModifier_annotation_ann) => hydra.ext.java.serde.writeAnnotation(v_MethodModifier_annotation_ann)
  case hydra.ext.java.syntax.MethodModifier.public => hydra.serialization.cst("public")
  case hydra.ext.java.syntax.MethodModifier.`protected` => hydra.serialization.cst("protected")
  case hydra.ext.java.syntax.MethodModifier.`private` => hydra.serialization.cst("private")
  case hydra.ext.java.syntax.MethodModifier.`abstract` => hydra.serialization.cst("abstract")
  case hydra.ext.java.syntax.MethodModifier.`final` => hydra.serialization.cst("final")
  case hydra.ext.java.syntax.MethodModifier.synchronized => hydra.serialization.cst("synchronized")
  case hydra.ext.java.syntax.MethodModifier.native => hydra.serialization.cst("native")
  case hydra.ext.java.syntax.MethodModifier.strictfb => hydra.serialization.cst("strictfb")

def writeMethodName(mn: hydra.ext.java.syntax.MethodName): hydra.ast.Expr = hydra.ext.java.serde.writeIdentifier(mn)

def writeMethodReference[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:MethodReference")

def writeMultiplicativeExpression(e: hydra.ext.java.syntax.MultiplicativeExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.MultiplicativeExpression.unary(v_MultiplicativeExpression_unary_u) => hydra.ext.java.serde.writeUnaryExpression(v_MultiplicativeExpression_unary_u)
  case hydra.ext.java.syntax.MultiplicativeExpression.times(v_MultiplicativeExpression_times_b) => hydra.serialization.infixWs("*")(hydra.ext.java.serde.writeMultiplicativeExpression(v_MultiplicativeExpression_times_b.lhs))(hydra.ext.java.serde.writeUnaryExpression(v_MultiplicativeExpression_times_b.rhs))
  case hydra.ext.java.syntax.MultiplicativeExpression.divide(v_MultiplicativeExpression_divide_b) => hydra.serialization.infixWs("/")(hydra.ext.java.serde.writeMultiplicativeExpression(v_MultiplicativeExpression_divide_b.lhs))(hydra.ext.java.serde.writeUnaryExpression(v_MultiplicativeExpression_divide_b.rhs))
  case hydra.ext.java.syntax.MultiplicativeExpression.mod(v_MultiplicativeExpression_mod_b) => hydra.serialization.infixWs("%")(hydra.ext.java.serde.writeMultiplicativeExpression(v_MultiplicativeExpression_mod_b.lhs))(hydra.ext.java.serde.writeUnaryExpression(v_MultiplicativeExpression_mod_b.rhs))

def writeNormalAnnotation(na: hydra.ext.java.syntax.NormalAnnotation): hydra.ast.Expr =
  {
  lazy val tname: hydra.ext.java.syntax.TypeName = (na.typeName)
  lazy val pairs: Seq[hydra.ext.java.syntax.ElementValuePair] = (na.pairs)
  hydra.serialization.prefix("@")(hydra.serialization.noSep(Seq(hydra.ext.java.serde.writeTypeName(tname), hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.ElementValuePair, hydra.ast.Expr](hydra.ext.java.serde.writeElementValuePair)(pairs)))))
}

def writeNormalClassDeclaration(ncd: hydra.ext.java.syntax.NormalClassDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.ClassModifier] = (ncd.modifiers)
  lazy val id: hydra.ext.java.syntax.TypeIdentifier = (ncd.identifier)
  lazy val tparams: Seq[hydra.ext.java.syntax.TypeParameter] = (ncd.parameters)
  lazy val msuperc: Option[hydra.ext.java.syntax.ClassType] = (ncd.`extends`)
  lazy val superi: Seq[hydra.ext.java.syntax.InterfaceType] = (ncd.implements)
  lazy val body: hydra.ext.java.syntax.ClassBody = (ncd.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.ClassModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.ClassModifier, hydra.ast.Expr](hydra.ext.java.serde.writeClassModifier)(mods)))), Some(hydra.serialization.cst("class")), Some(hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.java.serde.writeTypeIdentifier(id)), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter, hydra.ast.Expr](hydra.ext.java.serde.writeTypeParameter)(tparams)))))))), hydra.lib.maybes.map[hydra.ext.java.syntax.ClassType, hydra.ast.Expr]((c: hydra.ext.java.syntax.ClassType) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("extends"), hydra.ext.java.serde.writeClassType(c))))(msuperc), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.InterfaceType](superi))(None)(Some(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("implements"), hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.InterfaceType, hydra.ast.Expr](hydra.ext.java.serde.writeInterfaceType)(superi)))))), Some(hydra.ext.java.serde.writeClassBody(body)))))
}

def writeNormalInterfaceDeclaration(nid: hydra.ext.java.syntax.NormalInterfaceDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.InterfaceModifier] = (nid.modifiers)
  lazy val id: hydra.ext.java.syntax.TypeIdentifier = (nid.identifier)
  lazy val tparams: Seq[hydra.ext.java.syntax.TypeParameter] = (nid.parameters)
  lazy val `extends`: Seq[hydra.ext.java.syntax.InterfaceType] = (nid.`extends`)
  lazy val body: hydra.ext.java.syntax.InterfaceBody = (nid.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.InterfaceModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.InterfaceModifier, hydra.ast.Expr](hydra.ext.java.serde.writeInterfaceModifier)(mods)))), Some(hydra.serialization.cst("interface")), Some(hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.java.serde.writeTypeIdentifier(id)), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter, hydra.ast.Expr](hydra.ext.java.serde.writeTypeParameter)(tparams)))))))), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.InterfaceType](`extends`))(None)(Some(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("extends"), hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.InterfaceType, hydra.ast.Expr](hydra.ext.java.serde.writeInterfaceType)(`extends`)))))), Some(hydra.ext.java.serde.writeInterfaceBody(body)))))
}

def writeNumericType(nt: hydra.ext.java.syntax.NumericType): hydra.ast.Expr =
  nt match
  case hydra.ext.java.syntax.NumericType.integral(v_NumericType_integral_it) => hydra.ext.java.serde.writeIntegralType(v_NumericType_integral_it)
  case hydra.ext.java.syntax.NumericType.floatingPoint(v_NumericType_floatingPoint_ft) => hydra.ext.java.serde.writeFloatingPointType(v_NumericType_floatingPoint_ft)

def writePackageDeclaration(pd: hydra.ext.java.syntax.PackageDeclaration): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.PackageModifier] = (pd.modifiers)
  lazy val ids: Seq[hydra.ext.java.syntax.Identifier] = (pd.identifiers)
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.PackageModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.PackageModifier, hydra.ast.Expr](hydra.ext.java.serde.writePackageModifier)(mods)))), Some(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("package"), hydra.serialization.cst(hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[hydra.ext.java.syntax.Identifier, scala.Predef.String]((id: hydra.ext.java.syntax.Identifier) => id)(ids))))))))))
}

def writePackageName(pn: hydra.ext.java.syntax.PackageName): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.ext.java.syntax.Identifier, hydra.ast.Expr](hydra.ext.java.serde.writeIdentifier)(pn))

def writePackageOrTypeName(potn: hydra.ext.java.syntax.PackageOrTypeName): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.ext.java.syntax.Identifier, hydra.ast.Expr](hydra.ext.java.serde.writeIdentifier)(potn))

def writePackageModifier(pm: hydra.ext.java.syntax.PackageModifier): hydra.ast.Expr = hydra.ext.java.serde.writeAnnotation(pm)

def writePostDecrementExpression[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:PostDecrementExpression")

def writePostIncrementExpression[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:PostIncrementExpression")

def writePostfixExpression(e: hydra.ext.java.syntax.PostfixExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.PostfixExpression.primary(v_PostfixExpression_primary_p) => hydra.ext.java.serde.writePrimary(v_PostfixExpression_primary_p)
  case hydra.ext.java.syntax.PostfixExpression.name(v_PostfixExpression_name_en) => hydra.ext.java.serde.writeExpressionName(v_PostfixExpression_name_en)
  case hydra.ext.java.syntax.PostfixExpression.postIncrement(v_PostfixExpression_postIncrement_pi) => hydra.ext.java.serde.writePostIncrementExpression(v_PostfixExpression_postIncrement_pi)
  case hydra.ext.java.syntax.PostfixExpression.postDecrement(v_PostfixExpression_postDecrement_pd) => hydra.ext.java.serde.writePostDecrementExpression(v_PostfixExpression_postDecrement_pd)

def writePreDecrementExpression[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:PreDecrementExpression")

def writePreIncrementExpression[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:PreIncrementExpression")

def writePrimary(p: hydra.ext.java.syntax.Primary): hydra.ast.Expr =
  p match
  case hydra.ext.java.syntax.Primary.noNewArray(v_Primary_noNewArray_n) => hydra.ext.java.serde.writePrimaryNoNewArray(v_Primary_noNewArray_n)
  case hydra.ext.java.syntax.Primary.arrayCreation(v_Primary_arrayCreation_a) => hydra.ext.java.serde.writeArrayCreationExpression(v_Primary_arrayCreation_a)

def writePrimaryNoNewArray(p: hydra.ext.java.syntax.PrimaryNoNewArray): hydra.ast.Expr =
  p match
  case hydra.ext.java.syntax.PrimaryNoNewArray.literal(v_PrimaryNoNewArray_literal_l) => hydra.ext.java.serde.writeLiteral(v_PrimaryNoNewArray_literal_l)
  case hydra.ext.java.syntax.PrimaryNoNewArray.classLiteral(v_PrimaryNoNewArray_classLiteral_cl) => hydra.ext.java.serde.writeClassLiteral(v_PrimaryNoNewArray_classLiteral_cl)
  case hydra.ext.java.syntax.PrimaryNoNewArray.`this` => hydra.serialization.cst("this")
  case hydra.ext.java.syntax.PrimaryNoNewArray.dotThis(v_PrimaryNoNewArray_dotThis_n) => hydra.serialization.dotSep(Seq(hydra.ext.java.serde.writeTypeName(v_PrimaryNoNewArray_dotThis_n), hydra.serialization.cst("this")))
  case hydra.ext.java.syntax.PrimaryNoNewArray.parens(v_PrimaryNoNewArray_parens_e) => hydra.serialization.parenList(false)(Seq(hydra.ext.java.serde.writeExpression(v_PrimaryNoNewArray_parens_e)))
  case hydra.ext.java.syntax.PrimaryNoNewArray.classInstance(v_PrimaryNoNewArray_classInstance_ci) => hydra.ext.java.serde.writeClassInstanceCreationExpression(v_PrimaryNoNewArray_classInstance_ci)
  case hydra.ext.java.syntax.PrimaryNoNewArray.fieldAccess(v_PrimaryNoNewArray_fieldAccess_fa) => hydra.ext.java.serde.writeFieldAccess(v_PrimaryNoNewArray_fieldAccess_fa)
  case hydra.ext.java.syntax.PrimaryNoNewArray.arrayAccess(v_PrimaryNoNewArray_arrayAccess_aa) => hydra.ext.java.serde.writeArrayAccess(v_PrimaryNoNewArray_arrayAccess_aa)
  case hydra.ext.java.syntax.PrimaryNoNewArray.methodInvocation(v_PrimaryNoNewArray_methodInvocation_mi) => hydra.ext.java.serde.writeMethodInvocation(v_PrimaryNoNewArray_methodInvocation_mi)
  case hydra.ext.java.syntax.PrimaryNoNewArray.methodReference(v_PrimaryNoNewArray_methodReference_mr) => hydra.ext.java.serde.writeMethodReference(v_PrimaryNoNewArray_methodReference_mr)

def writePrimitiveType(pt: hydra.ext.java.syntax.PrimitiveType): hydra.ast.Expr =
  pt match
  case hydra.ext.java.syntax.PrimitiveType.numeric(v_PrimitiveType_numeric_nt) => hydra.ext.java.serde.writeNumericType(v_PrimitiveType_numeric_nt)
  case hydra.ext.java.syntax.PrimitiveType.boolean => hydra.serialization.cst("boolean")

def writePrimitiveTypeWithAnnotations(ptwa: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations): hydra.ast.Expr =
  {
  lazy val pt: hydra.ext.java.syntax.PrimitiveType = (ptwa.`type`)
  lazy val anns: Seq[hydra.ext.java.syntax.Annotation] = (ptwa.annotations)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.Annotation, hydra.ast.Expr](hydra.ext.java.serde.writeAnnotation)(anns)))), Some(hydra.ext.java.serde.writePrimitiveType(pt)))))
}

def writeReceiverParameter[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:ReceiverParameter")

def writeReferenceType(rt: hydra.ext.java.syntax.ReferenceType): hydra.ast.Expr =
  rt match
  case hydra.ext.java.syntax.ReferenceType.classOrInterface(v_ReferenceType_classOrInterface_cit) => hydra.ext.java.serde.writeClassOrInterfaceType(v_ReferenceType_classOrInterface_cit)
  case hydra.ext.java.syntax.ReferenceType.variable(v_ReferenceType_variable_v) => hydra.ext.java.serde.writeTypeVariable(v_ReferenceType_variable_v)
  case hydra.ext.java.syntax.ReferenceType.array(v_ReferenceType_array_at) => hydra.ext.java.serde.writeArrayType(v_ReferenceType_array_at)

def writeRelationalExpression(e: hydra.ext.java.syntax.RelationalExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.RelationalExpression.simple(v_RelationalExpression_simple_s) => hydra.ext.java.serde.writeShiftExpression(v_RelationalExpression_simple_s)
  case hydra.ext.java.syntax.RelationalExpression.lessThan(v_RelationalExpression_lessThan_lt) => hydra.ext.java.serde.writeRelationalExpression_LessThan(v_RelationalExpression_lessThan_lt)
  case hydra.ext.java.syntax.RelationalExpression.greaterThan(v_RelationalExpression_greaterThan_gt) => hydra.ext.java.serde.writeRelationalExpression_GreaterThan(v_RelationalExpression_greaterThan_gt)
  case hydra.ext.java.syntax.RelationalExpression.lessThanEqual(v_RelationalExpression_lessThanEqual_lte) => hydra.ext.java.serde.writeRelationalExpression_LessThanEqual(v_RelationalExpression_lessThanEqual_lte)
  case hydra.ext.java.syntax.RelationalExpression.greaterThanEqual(v_RelationalExpression_greaterThanEqual_gte) => hydra.ext.java.serde.writeRelationalExpression_GreaterThanEqual(v_RelationalExpression_greaterThanEqual_gte)
  case hydra.ext.java.syntax.RelationalExpression.instanceof(v_RelationalExpression_instanceof_i) => hydra.ext.java.serde.writeRelationalExpression_InstanceOf(v_RelationalExpression_instanceof_i)

def writeRelationalExpression_GreaterThan(gt: hydra.ext.java.syntax.RelationalExpression_GreaterThan): hydra.ast.Expr =
  hydra.serialization.infixWs(">")(hydra.ext.java.serde.writeRelationalExpression(gt.lhs))(hydra.ext.java.serde.writeShiftExpression(gt.rhs))

def writeRelationalExpression_GreaterThanEqual(gte: hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual): hydra.ast.Expr =
  hydra.serialization.infixWs(">=")(hydra.ext.java.serde.writeRelationalExpression(gte.lhs))(hydra.ext.java.serde.writeShiftExpression(gte.rhs))

def writeRelationalExpression_InstanceOf(io: hydra.ext.java.syntax.RelationalExpression_InstanceOf): hydra.ast.Expr =
  hydra.serialization.infixWs("instanceof")(hydra.ext.java.serde.writeRelationalExpression(io.lhs))(hydra.ext.java.serde.writeReferenceType(io.rhs))

def writeRelationalExpression_LessThan(lt: hydra.ext.java.syntax.RelationalExpression_LessThan): hydra.ast.Expr =
  hydra.serialization.infixWs("<")(hydra.ext.java.serde.writeRelationalExpression(lt.lhs))(hydra.ext.java.serde.writeShiftExpression(lt.rhs))

def writeRelationalExpression_LessThanEqual(lte: hydra.ext.java.syntax.RelationalExpression_LessThanEqual): hydra.ast.Expr =
  hydra.serialization.infixWs("<=")(hydra.ext.java.serde.writeRelationalExpression(lte.lhs))(hydra.ext.java.serde.writeShiftExpression(lte.rhs))

def writeResult(r: hydra.ext.java.syntax.Result): hydra.ast.Expr =
  r match
  case hydra.ext.java.syntax.Result.`type`(v_Result_type_t) => hydra.ext.java.serde.writeUnannType(v_Result_type_t)
  case hydra.ext.java.syntax.Result.void => hydra.serialization.cst("void")

def writeReturnStatement(rs: hydra.ext.java.syntax.ReturnStatement): hydra.ast.Expr =
  {
  lazy val mex: Option[hydra.ext.java.syntax.Expression] = rs
  hydra.serialization.withSemi(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("return")), hydra.lib.maybes.map[hydra.ext.java.syntax.Expression, hydra.ast.Expr](hydra.ext.java.serde.writeExpression)(mex)))))
}

def writeShiftExpression(e: hydra.ext.java.syntax.ShiftExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.ShiftExpression.unary(v_ShiftExpression_unary_a) => hydra.ext.java.serde.writeAdditiveExpression(v_ShiftExpression_unary_a)
  case hydra.ext.java.syntax.ShiftExpression.shiftLeft(v_ShiftExpression_shiftLeft_b) => hydra.serialization.infixWs("<<")(hydra.ext.java.serde.writeShiftExpression(v_ShiftExpression_shiftLeft_b.lhs))(hydra.ext.java.serde.writeAdditiveExpression(v_ShiftExpression_shiftLeft_b.rhs))
  case hydra.ext.java.syntax.ShiftExpression.shiftRight(v_ShiftExpression_shiftRight_b) => hydra.serialization.infixWs(">>")(hydra.ext.java.serde.writeShiftExpression(v_ShiftExpression_shiftRight_b.lhs))(hydra.ext.java.serde.writeAdditiveExpression(v_ShiftExpression_shiftRight_b.rhs))
  case hydra.ext.java.syntax.ShiftExpression.shiftRightZeroFill(v_ShiftExpression_shiftRightZeroFill_b) => hydra.serialization.infixWs(">>>")(hydra.ext.java.serde.writeShiftExpression(v_ShiftExpression_shiftRightZeroFill_b.lhs))(hydra.ext.java.serde.writeAdditiveExpression(v_ShiftExpression_shiftRightZeroFill_b.rhs))

def writeSimpleTypeName(stn: hydra.ext.java.syntax.SimpleTypeName): hydra.ast.Expr = hydra.ext.java.serde.writeTypeIdentifier(stn)

def writeSingleElementAnnotation(sea: hydra.ext.java.syntax.SingleElementAnnotation): hydra.ast.Expr =
  {
  lazy val tname: hydra.ext.java.syntax.TypeName = (sea.name)
  lazy val mv: Option[hydra.ext.java.syntax.ElementValue] = (sea.value)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.java.syntax.ElementValue](hydra.ext.java.serde.writeMarkerAnnotation(tname))((v: hydra.ext.java.syntax.ElementValue) =>
    hydra.serialization.prefix("@")(hydra.serialization.noSep(Seq(hydra.ext.java.serde.writeTypeName(tname), hydra.serialization.parenList(false)(Seq(hydra.ext.java.serde.writeElementValue(v)))))))(mv)
}

def writeStatement(s: hydra.ext.java.syntax.Statement): hydra.ast.Expr =
  s match
  case hydra.ext.java.syntax.Statement.withoutTrailing(v_Statement_withoutTrailing_s2) => hydra.ext.java.serde.writeStatementWithoutTrailingSubstatement(v_Statement_withoutTrailing_s2)
  case hydra.ext.java.syntax.Statement.labeled(v_Statement_labeled_l) => hydra.ext.java.serde.writeLabeledStatement(v_Statement_labeled_l)
  case hydra.ext.java.syntax.Statement.ifThen(v_Statement_ifThen_it) => hydra.ext.java.serde.writeIfThenStatement(v_Statement_ifThen_it)
  case hydra.ext.java.syntax.Statement.ifThenElse(v_Statement_ifThenElse_ite) => hydra.ext.java.serde.writeIfThenElseStatement(v_Statement_ifThenElse_ite)
  case hydra.ext.java.syntax.Statement.`while`(v_Statement_while_w) => hydra.ext.java.serde.writeWhileStatement(v_Statement_while_w)
  case hydra.ext.java.syntax.Statement.`for`(v_Statement_for_f) => hydra.ext.java.serde.writeForStatement(v_Statement_for_f)

def writeStatementExpression(e: hydra.ext.java.syntax.StatementExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.StatementExpression.assignment(v_StatementExpression_assignment_a) => hydra.ext.java.serde.writeAssignment(v_StatementExpression_assignment_a)
  case hydra.ext.java.syntax.StatementExpression.preIncrement(v_StatementExpression_preIncrement_pi) => hydra.ext.java.serde.writePreIncrementExpression(v_StatementExpression_preIncrement_pi)
  case hydra.ext.java.syntax.StatementExpression.preDecrement(v_StatementExpression_preDecrement_pd) => hydra.ext.java.serde.writePreDecrementExpression(v_StatementExpression_preDecrement_pd)
  case hydra.ext.java.syntax.StatementExpression.postIncrement(v_StatementExpression_postIncrement_pi) => hydra.ext.java.serde.writePostIncrementExpression(v_StatementExpression_postIncrement_pi)
  case hydra.ext.java.syntax.StatementExpression.postDecrement(v_StatementExpression_postDecrement_pd) => hydra.ext.java.serde.writePostDecrementExpression(v_StatementExpression_postDecrement_pd)
  case hydra.ext.java.syntax.StatementExpression.methodInvocation(v_StatementExpression_methodInvocation_m) => hydra.ext.java.serde.writeMethodInvocation(v_StatementExpression_methodInvocation_m)
  case hydra.ext.java.syntax.StatementExpression.classInstanceCreation(v_StatementExpression_classInstanceCreation_cic) => hydra.ext.java.serde.writeClassInstanceCreationExpression(v_StatementExpression_classInstanceCreation_cic)

def writeStatementWithoutTrailingSubstatement(s: hydra.ext.java.syntax.StatementWithoutTrailingSubstatement): hydra.ast.Expr =
  s match
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.block(v_StatementWithoutTrailingSubstatement_block_b) => hydra.ext.java.serde.writeBlock(v_StatementWithoutTrailingSubstatement_block_b)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.empty => hydra.serialization.cst(";")
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.expression(v_StatementWithoutTrailingSubstatement_expression_e) => hydra.ext.java.serde.writeExpressionStatement(v_StatementWithoutTrailingSubstatement_expression_e)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.assert(v_StatementWithoutTrailingSubstatement_assert_a) => hydra.ext.java.serde.writeAssertStatement(v_StatementWithoutTrailingSubstatement_assert_a)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.switch(v_StatementWithoutTrailingSubstatement_switch_s2) => hydra.ext.java.serde.writeSwitchStatement(v_StatementWithoutTrailingSubstatement_switch_s2)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.`do`(v_StatementWithoutTrailingSubstatement_do_d) => hydra.ext.java.serde.writeDoStatement(v_StatementWithoutTrailingSubstatement_do_d)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.break(v_StatementWithoutTrailingSubstatement_break_b) => hydra.ext.java.serde.writeBreakStatement(v_StatementWithoutTrailingSubstatement_break_b)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.continue(v_StatementWithoutTrailingSubstatement_continue_c) => hydra.ext.java.serde.writeContinueStatement(v_StatementWithoutTrailingSubstatement_continue_c)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.`return`(v_StatementWithoutTrailingSubstatement_return_r) => hydra.ext.java.serde.writeReturnStatement(v_StatementWithoutTrailingSubstatement_return_r)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.synchronized(v_StatementWithoutTrailingSubstatement_synchronized_s2) => hydra.ext.java.serde.writeSynchronizedStatement(v_StatementWithoutTrailingSubstatement_synchronized_s2)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.`throw`(v_StatementWithoutTrailingSubstatement_throw_t) => hydra.ext.java.serde.writeThrowStatement(v_StatementWithoutTrailingSubstatement_throw_t)
  case hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.`try`(v_StatementWithoutTrailingSubstatement_try_t) => hydra.ext.java.serde.writeTryStatement(v_StatementWithoutTrailingSubstatement_try_t)

def writeStaticInitializer[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:StaticInitializer")

def writeStringLiteral(sl: hydra.ext.java.syntax.StringLiteral): hydra.ast.Expr =
  {
  lazy val s: scala.Predef.String = sl
  hydra.serialization.cst(hydra.lib.strings.cat2("\"")(hydra.lib.strings.cat2(hydra.ext.java.serde.escapeJavaString(s))("\"")))
}

def writeSwitchStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:SwitchStatement")

def writeSynchronizedStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:SynchronizedStatement")

def writeThrowStatement(ts: hydra.ext.java.syntax.ThrowStatement): hydra.ast.Expr =
  hydra.serialization.withSemi(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("throw"), hydra.ext.java.serde.writeExpression(ts))))

def writeThrows[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:Throws")

def writeTryStatement[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:TryStatement")

def writeType(t: hydra.ext.java.syntax.Type): hydra.ast.Expr =
  t match
  case hydra.ext.java.syntax.Type.primitive(v_Type_primitive_pt) => hydra.ext.java.serde.writePrimitiveTypeWithAnnotations(v_Type_primitive_pt)
  case hydra.ext.java.syntax.Type.reference(v_Type_reference_rt) => hydra.ext.java.serde.writeReferenceType(v_Type_reference_rt)

def writeTypeArgument(a: hydra.ext.java.syntax.TypeArgument): hydra.ast.Expr =
  a match
  case hydra.ext.java.syntax.TypeArgument.reference(v_TypeArgument_reference_rt) => hydra.ext.java.serde.writeReferenceType(v_TypeArgument_reference_rt)
  case hydra.ext.java.syntax.TypeArgument.wildcard(v_TypeArgument_wildcard_w) => hydra.ext.java.serde.writeWildcard(v_TypeArgument_wildcard_w)

def writeTypeArgumentsOrDiamond(targs: hydra.ext.java.syntax.TypeArgumentsOrDiamond): hydra.ast.Expr =
  targs match
  case hydra.ext.java.syntax.TypeArgumentsOrDiamond.arguments(v_TypeArgumentsOrDiamond_arguments_args) => hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.TypeArgument, hydra.ast.Expr](hydra.ext.java.serde.writeTypeArgument)(v_TypeArgumentsOrDiamond_arguments_args))
  case hydra.ext.java.syntax.TypeArgumentsOrDiamond.diamond => hydra.serialization.cst("<>")

def writeTypeBound(b: hydra.ext.java.syntax.TypeBound): hydra.ast.Expr =
  b match
  case hydra.ext.java.syntax.TypeBound.variable(v_TypeBound_variable_tv) => hydra.ext.java.serde.writeTypeVariable(v_TypeBound_variable_tv)
  case hydra.ext.java.syntax.TypeBound.classOrInterface(v_TypeBound_classOrInterface_ci) => {
    lazy val cit: hydra.ext.java.syntax.ClassOrInterfaceType = (v_TypeBound_classOrInterface_ci.`type`)
    lazy val additional: Seq[hydra.ext.java.syntax.AdditionalBound] = (v_TypeBound_classOrInterface_ci.additional)
    hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.lists.`null`[hydra.ext.java.syntax.AdditionalBound](additional))(hydra.ext.java.serde.writeClassOrInterfaceType(cit))(hydra.serialization.spaceSep(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.java.serde.writeClassOrInterfaceType(cit))(hydra.lib.lists.map[hydra.ext.java.syntax.AdditionalBound, hydra.ast.Expr](hydra.ext.java.serde.writeAdditionalBound)(additional))))
  }

def writeTypeDeclaration(d: hydra.ext.java.syntax.TypeDeclaration): hydra.ast.Expr =
  d match
  case hydra.ext.java.syntax.TypeDeclaration.`class`(v_TypeDeclaration_class_d2) => hydra.ext.java.serde.writeClassDeclaration(v_TypeDeclaration_class_d2)
  case hydra.ext.java.syntax.TypeDeclaration.interface(v_TypeDeclaration_interface_d2) => hydra.ext.java.serde.writeInterfaceDeclaration(v_TypeDeclaration_interface_d2)
  case hydra.ext.java.syntax.TypeDeclaration.none => hydra.serialization.cst(";")

def writeTypeDeclarationWithComments(tdwc: hydra.ext.java.syntax.TypeDeclarationWithComments): hydra.ast.Expr =
  {
  lazy val d: hydra.ext.java.syntax.TypeDeclaration = (tdwc.value)
  lazy val mc: Option[scala.Predef.String] = (tdwc.comments)
  hydra.ext.java.serde.withComments(mc)(hydra.ext.java.serde.writeTypeDeclaration(d))
}

def writeTypeIdentifier(tid: hydra.ext.java.syntax.TypeIdentifier): hydra.ast.Expr = hydra.ext.java.serde.writeIdentifier(tid)

def writeTypeName(tn: hydra.ext.java.syntax.TypeName): hydra.ast.Expr =
  {
  lazy val id: hydra.ext.java.syntax.TypeIdentifier = (tn.identifier)
  lazy val mqual: Option[hydra.ext.java.syntax.PackageOrTypeName] = (tn.qualifier)
  hydra.serialization.dotSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.ext.java.syntax.PackageOrTypeName, hydra.ast.Expr](hydra.ext.java.serde.writePackageOrTypeName)(mqual), Some(hydra.ext.java.serde.writeTypeIdentifier(id)))))
}

def writeTypeParameter(tp: hydra.ext.java.syntax.TypeParameter): hydra.ast.Expr =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.TypeParameterModifier] = (tp.modifiers)
  lazy val id: hydra.ext.java.syntax.TypeIdentifier = (tp.identifier)
  lazy val bound: Option[hydra.ext.java.syntax.TypeBound] = (tp.bound)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeParameterModifier](mods))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameterModifier, hydra.ast.Expr](hydra.ext.java.serde.writeTypeParameterModifier)(mods)))), Some(hydra.ext.java.serde.writeTypeIdentifier(id)), hydra.lib.maybes.map[hydra.ext.java.syntax.TypeBound, hydra.ast.Expr]((b: hydra.ext.java.syntax.TypeBound) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("extends"), hydra.ext.java.serde.writeTypeBound(b))))(bound))))
}

def writeTypeParameterModifier(tpm: hydra.ext.java.syntax.TypeParameterModifier): hydra.ast.Expr = hydra.ext.java.serde.writeAnnotation(tpm)

def writeTypeVariable(tv: hydra.ext.java.syntax.TypeVariable): hydra.ast.Expr =
  {
  lazy val anns: Seq[hydra.ext.java.syntax.Annotation] = (tv.annotations)
  lazy val id: hydra.ext.java.syntax.TypeIdentifier = (tv.identifier)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.spaceSep(hydra.lib.lists.map[hydra.ext.java.syntax.Annotation, hydra.ast.Expr](hydra.ext.java.serde.writeAnnotation)(anns)))), Some(hydra.ext.java.serde.writeTypeIdentifier(id)))))
}

def writeUnannType(ut: hydra.ext.java.syntax.UnannType): hydra.ast.Expr = hydra.ext.java.serde.writeType(ut)

def writeUnaryExpression(e: hydra.ext.java.syntax.UnaryExpression): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.UnaryExpression.preIncrement(v_UnaryExpression_preIncrement_pi) => hydra.ext.java.serde.writePreIncrementExpression(v_UnaryExpression_preIncrement_pi)
  case hydra.ext.java.syntax.UnaryExpression.preDecrement(v_UnaryExpression_preDecrement_pd) => hydra.ext.java.serde.writePreDecrementExpression(v_UnaryExpression_preDecrement_pd)
  case hydra.ext.java.syntax.UnaryExpression.plus(v_UnaryExpression_plus_p) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("+"), hydra.ext.java.serde.writeUnaryExpression(v_UnaryExpression_plus_p)))
  case hydra.ext.java.syntax.UnaryExpression.minus(v_UnaryExpression_minus_m) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("-"), hydra.ext.java.serde.writeUnaryExpression(v_UnaryExpression_minus_m)))
  case hydra.ext.java.syntax.UnaryExpression.other(v_UnaryExpression_other_o) => hydra.ext.java.serde.writeUnaryExpressionNotPlusMinus(v_UnaryExpression_other_o)

def writeUnaryExpressionNotPlusMinus(e: hydra.ext.java.syntax.UnaryExpressionNotPlusMinus): hydra.ast.Expr =
  e match
  case hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(v_UnaryExpressionNotPlusMinus_postfix_p) => hydra.ext.java.serde.writePostfixExpression(v_UnaryExpressionNotPlusMinus_postfix_p)
  case hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.tilde(v_UnaryExpressionNotPlusMinus_tilde_u) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("~"), hydra.ext.java.serde.writeUnaryExpression(v_UnaryExpressionNotPlusMinus_tilde_u)))
  case hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.not(v_UnaryExpressionNotPlusMinus_not_u) => hydra.serialization.noSep(Seq(hydra.serialization.cst("!"), hydra.ext.java.serde.writeUnaryExpression(v_UnaryExpressionNotPlusMinus_not_u)))
  case hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.cast(v_UnaryExpressionNotPlusMinus_cast_c) => hydra.ext.java.serde.writeCastExpression(v_UnaryExpressionNotPlusMinus_cast_c)

def writeUnqualifiedClassInstanceCreationExpression(ucice: hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression): hydra.ast.Expr =
  {
  lazy val targs: Seq[hydra.ext.java.syntax.TypeArgument] = (ucice.typeArguments)
  lazy val cit: hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate = (ucice.classOrInterface)
  lazy val args: Seq[hydra.ext.java.syntax.Expression] = (ucice.arguments)
  lazy val mbody: Option[hydra.ext.java.syntax.ClassBody] = (ucice.body)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("new")), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeArgument](targs))(None)(Some(hydra.serialization.angleBracesList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.TypeArgument, hydra.ast.Expr](hydra.ext.java.serde.writeTypeArgument)(targs)))), Some(hydra.serialization.noSep(Seq(hydra.ext.java.serde.writeClassOrInterfaceTypeToInstantiate(cit), hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.java.syntax.Expression, hydra.ast.Expr](hydra.ext.java.serde.writeExpression)(args))))), hydra.lib.maybes.map[hydra.ext.java.syntax.ClassBody, hydra.ast.Expr](hydra.ext.java.serde.writeClassBody)(mbody))))
}

def writeVariableArityParameter[T0](_x: T0): hydra.ast.Expr = hydra.serialization.cst("STUB:VariableArityParameter")

def writeVariableDeclarator(vd: hydra.ext.java.syntax.VariableDeclarator): hydra.ast.Expr =
  {
  lazy val id: hydra.ext.java.syntax.VariableDeclaratorId = (vd.id)
  lazy val minit: Option[hydra.ext.java.syntax.VariableInitializer] = (vd.initializer)
  lazy val idSec: hydra.ast.Expr = hydra.ext.java.serde.writeVariableDeclaratorId(id)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.java.syntax.VariableInitializer](idSec)((init: hydra.ext.java.syntax.VariableInitializer) =>
    hydra.serialization.infixWs("=")(idSec)(hydra.ext.java.serde.writeVariableInitializer(init)))(minit)
}

def writeVariableDeclaratorId(vdi: hydra.ext.java.syntax.VariableDeclaratorId): hydra.ast.Expr =
  {
  lazy val id: hydra.ext.java.syntax.Identifier = (vdi.identifier)
  lazy val mdims: Option[hydra.ext.java.syntax.Dims] = (vdi.dims)
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.java.serde.writeIdentifier(id)), hydra.lib.maybes.map[hydra.ext.java.syntax.Dims, hydra.ast.Expr](hydra.ext.java.serde.writeDims)(mdims))))
}

def writeVariableInitializer(i: hydra.ext.java.syntax.VariableInitializer): hydra.ast.Expr =
  i match
  case hydra.ext.java.syntax.VariableInitializer.expression(v_VariableInitializer_expression_e) => hydra.ext.java.serde.writeExpression(v_VariableInitializer_expression_e)
  case hydra.ext.java.syntax.VariableInitializer.arrayInitializer(v_VariableInitializer_arrayInitializer_ai) => hydra.ext.java.serde.writeArrayInitializer(v_VariableInitializer_arrayInitializer_ai)

def writeVariableModifier(m: hydra.ext.java.syntax.VariableModifier): hydra.ast.Expr =
  m match
  case hydra.ext.java.syntax.VariableModifier.annotation(v_VariableModifier_annotation_ann) => hydra.ext.java.serde.writeAnnotation(v_VariableModifier_annotation_ann)
  case hydra.ext.java.syntax.VariableModifier.`final` => hydra.serialization.cst("final")

def writeWhileStatement(ws: hydra.ext.java.syntax.WhileStatement): hydra.ast.Expr =
  {
  lazy val mcond: Option[hydra.ext.java.syntax.Expression] = (ws.cond)
  lazy val body: hydra.ext.java.syntax.Statement = (ws.body)
  lazy val condSer: hydra.ast.Expr = hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.java.syntax.Expression](hydra.serialization.cst("true"))((c: hydra.ext.java.syntax.Expression) => hydra.ext.java.serde.writeExpression(c))(mcond)
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("while"), hydra.serialization.parenList(false)(Seq(condSer)), hydra.serialization.curlyBlock(hydra.serialization.fullBlockStyle)(hydra.ext.java.serde.writeStatement(body))))
}

def writeWildcard(w: hydra.ext.java.syntax.Wildcard): hydra.ast.Expr =
  {
  lazy val anns: Seq[hydra.ext.java.syntax.Annotation] = (w.annotations)
  lazy val mbounds: Option[hydra.ext.java.syntax.WildcardBounds] = (w.wildcard)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.java.syntax.Annotation](anns))(None)(Some(hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.java.syntax.Annotation, hydra.ast.Expr](hydra.ext.java.serde.writeAnnotation)(anns)))), Some(hydra.serialization.cst("*")), hydra.lib.maybes.map[hydra.ext.java.syntax.WildcardBounds, hydra.ast.Expr](hydra.ext.java.serde.writeWildcardBounds)(mbounds))))
}

def writeWildcardBounds(b: hydra.ext.java.syntax.WildcardBounds): hydra.ast.Expr =
  b match
  case hydra.ext.java.syntax.WildcardBounds.`extends`(v_WildcardBounds_extends_rt) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("extends"), hydra.ext.java.serde.writeReferenceType(v_WildcardBounds_extends_rt)))
  case hydra.ext.java.syntax.WildcardBounds.`super`(v_WildcardBounds_super_rt) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("super"), hydra.ext.java.serde.writeReferenceType(v_WildcardBounds_super_rt)))

def sanitizeJavaComment(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.intercalate("&gt;")(hydra.lib.strings.splitOn(">")(hydra.lib.strings.intercalate("&lt;")(hydra.lib.strings.splitOn("<")(s))))

def singleLineComment(c: scala.Predef.String): hydra.ast.Expr =
  hydra.serialization.cst(hydra.lib.strings.cat2("// ")(hydra.ext.java.serde.sanitizeJavaComment(c)))

def withComments(mc: Option[scala.Predef.String])(expr: hydra.ast.Expr): hydra.ast.Expr =
  hydra.lib.maybes.maybe[hydra.ast.Expr, scala.Predef.String](expr)((c: scala.Predef.String) =>
  hydra.serialization.newlineSep(Seq(hydra.serialization.cst(hydra.lib.strings.cat2("/**\n")(hydra.lib.strings.cat2(hydra.lib.strings.intercalate("\n")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((l: scala.Predef.String) => hydra.lib.strings.cat2(" * ")(l))(hydra.lib.strings.lines(hydra.ext.java.serde.sanitizeJavaComment(c)))))("\n */"))), expr)))(mc)
