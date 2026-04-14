package hydra.java.utils

import hydra.core.*

import hydra.errors.*

import hydra.java.environment.*

import hydra.java.syntax.*

import hydra.packaging.*

def addExpressions(exprs: Seq[hydra.java.syntax.MultiplicativeExpression]): hydra.java.syntax.AdditiveExpression =
  {
  lazy val first: hydra.java.syntax.AdditiveExpression = hydra.java.syntax.AdditiveExpression.unary(hydra.lib.lists.head[hydra.java.syntax.MultiplicativeExpression](exprs))
  lazy val rest: Seq[hydra.java.syntax.MultiplicativeExpression] = hydra.lib.lists.tail[hydra.java.syntax.MultiplicativeExpression](exprs)
  hydra.lib.lists.foldl[hydra.java.syntax.AdditiveExpression, hydra.java.syntax.MultiplicativeExpression]((ae: hydra.java.syntax.AdditiveExpression) =>
    (me: hydra.java.syntax.MultiplicativeExpression) =>
    hydra.java.syntax.AdditiveExpression.plus(hydra.java.syntax.AdditiveExpression_Binary(ae, me)))(first)(rest)
}

def addInScopeVar(name: hydra.core.Name)(aliases: hydra.java.environment.Aliases): hydra.java.environment.Aliases =
  hydra.java.environment.Aliases(aliases.currentNamespace, (aliases.packages), (aliases.branchVars),
     (aliases.recursiveVars), (aliases.inScopeTypeParams), (aliases.polymorphicLocals),
     hydra.lib.sets.insert[hydra.core.Name](name)(aliases.inScopeJavaVars), (aliases.varRenames),
     (aliases.lambdaVars), (aliases.typeVarSubst), (aliases.trustedTypeVars), (aliases.methodCodomain),
     (aliases.thunkedVars))

def addInScopeVars(names: Seq[hydra.core.Name])(aliases: hydra.java.environment.Aliases): hydra.java.environment.Aliases =
  hydra.lib.lists.foldl[hydra.java.environment.Aliases, hydra.core.Name]((a: hydra.java.environment.Aliases) => (n: hydra.core.Name) => hydra.java.utils.addInScopeVar(n)(a))(aliases)(names)

def addJavaTypeParameter[T0](rt: hydra.java.syntax.ReferenceType)(t: hydra.java.syntax.Type)(cx: T0): Either[hydra.errors.Error,
   hydra.java.syntax.Type] =
  t match
  case hydra.java.syntax.Type.reference(v_Type_reference_rt1) => v_Type_reference_rt1 match
    case hydra.java.syntax.ReferenceType.classOrInterface(v_ReferenceType_classOrInterface_cit) => v_ReferenceType_classOrInterface_cit match
      case hydra.java.syntax.ClassOrInterfaceType.`class`(v_ClassOrInterfaceType_class_ct) => {
        lazy val anns: Seq[hydra.java.syntax.Annotation] = (v_ClassOrInterfaceType_class_ct.annotations)
        lazy val qual: hydra.java.syntax.ClassTypeQualifier = (v_ClassOrInterfaceType_class_ct.qualifier)
        lazy val id: hydra.java.syntax.TypeIdentifier = (v_ClassOrInterfaceType_class_ct.identifier)
        lazy val args: Seq[hydra.java.syntax.TypeArgument] = (v_ClassOrInterfaceType_class_ct.arguments)
        Right(hydra.java.syntax.Type.reference(hydra.java.syntax.ReferenceType.classOrInterface(hydra.java.syntax.ClassOrInterfaceType.`class`(hydra.java.syntax.ClassType(anns,
           qual, id, hydra.lib.lists.concat2[hydra.java.syntax.TypeArgument](args)(Seq(hydra.java.syntax.TypeArgument.reference(rt))))))))
      }
      case hydra.java.syntax.ClassOrInterfaceType.interface(v_ClassOrInterfaceType_interface__) => Left(hydra.errors.Error.other("expected a Java class type"))
    case hydra.java.syntax.ReferenceType.variable(v_ReferenceType_variable_tv) => Right(hydra.java.utils.javaTypeVariableToType(v_ReferenceType_variable_tv))
    case hydra.java.syntax.ReferenceType.array(v_ReferenceType_array__) => Left(hydra.errors.Error.other("expected a Java class or interface type, or a variable"))
  case hydra.java.syntax.Type.primitive(v_Type_primitive__) => Left(hydra.errors.Error.other("expected a reference type"))

def addVarRename(original: hydra.core.Name)(renamed: hydra.core.Name)(aliases: hydra.java.environment.Aliases): hydra.java.environment.Aliases =
  hydra.java.environment.Aliases(aliases.currentNamespace, (aliases.packages), (aliases.branchVars),
     (aliases.recursiveVars), (aliases.inScopeTypeParams), (aliases.polymorphicLocals),
     (aliases.inScopeJavaVars), hydra.lib.maps.insert[hydra.core.Name, hydra.core.Name](original)(renamed)(aliases.varRenames),
     (aliases.lambdaVars), (aliases.typeVarSubst), (aliases.trustedTypeVars), (aliases.methodCodomain),
     (aliases.thunkedVars))

def fieldExpression(varId: hydra.java.syntax.Identifier)(fieldId: hydra.java.syntax.Identifier): hydra.java.syntax.ExpressionName = hydra.java.syntax.ExpressionName(Some(Seq(varId)),
   fieldId)

def fieldNameToJavaExpression(fname: hydra.core.Name): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.name(hydra.java.utils.javaIdentifierToJavaExpressionName(hydra.java.utils.fieldNameToJavaIdentifier(fname))))))))))))))))))

def fieldNameToJavaIdentifier(fname: hydra.core.Name): hydra.java.syntax.Identifier = hydra.java.utils.javaIdentifier(fname)

def fieldNameToJavaVariableDeclarator(fname: hydra.core.Name): hydra.java.syntax.VariableDeclarator =
  hydra.java.utils.javaVariableDeclarator(hydra.java.utils.javaIdentifier(fname))(None)

def fieldNameToJavaVariableDeclaratorId(fname: hydra.core.Name): hydra.java.syntax.VariableDeclaratorId =
  hydra.java.utils.javaVariableDeclaratorId(hydra.java.utils.javaIdentifier(fname))

def finalVarDeclarationStatement(id: hydra.java.syntax.Identifier)(rhs: hydra.java.syntax.Expression): hydra.java.syntax.BlockStatement =
  hydra.java.syntax.BlockStatement.localVariableDeclaration(hydra.java.syntax.LocalVariableDeclaration(Seq(hydra.java.syntax.VariableModifier.`final`),
     hydra.java.syntax.LocalVariableType.`var`, Seq(hydra.java.utils.javaVariableDeclarator(id)(Some(hydra.java.syntax.VariableInitializer.expression(rhs))))))

def importAliasesForModule(mod: hydra.packaging.Module): hydra.java.environment.Aliases =
  hydra.java.environment.Aliases(mod.namespace, hydra.lib.maps.empty[hydra.packaging.Namespace,
     hydra.java.syntax.PackageName], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name],
     hydra.lib.sets.empty[hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name],
     hydra.lib.sets.empty[hydra.core.Name], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name], None, hydra.lib.sets.empty[hydra.core.Name])

def interfaceMethodDeclaration(mods: Seq[hydra.java.syntax.InterfaceMethodModifier])(tparams: Seq[hydra.java.syntax.TypeParameter])(methodName: scala.Predef.String)(params: Seq[hydra.java.syntax.FormalParameter])(result: hydra.java.syntax.Result)(stmts: Option[Seq[hydra.java.syntax.BlockStatement]]): hydra.java.syntax.InterfaceMemberDeclaration =
  hydra.java.syntax.InterfaceMemberDeclaration.interfaceMethod(hydra.java.syntax.InterfaceMethodDeclaration(mods,
     hydra.java.utils.javaMethodHeader(tparams)(methodName)(params)(result), hydra.java.utils.javaMethodBody(stmts)))

def isEscaped(s: scala.Predef.String): Boolean = hydra.lib.equality.equal[Int](hydra.lib.strings.charAt(0)(s))(36)

def javaAdditiveExpressionToJavaExpression(ae: hydra.java.syntax.AdditiveExpression): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(ae)))))))))))

def javaArrayCreation(primType: hydra.java.syntax.PrimitiveTypeWithAnnotations)(minit: Option[hydra.java.syntax.ArrayInitializer]): hydra.java.syntax.Expression =
  {
  lazy val `init_`: hydra.java.syntax.ArrayInitializer = hydra.lib.maybes.cases[hydra.java.syntax.ArrayInitializer,
     hydra.java.syntax.ArrayInitializer](minit)(Seq())((i: hydra.java.syntax.ArrayInitializer) => i)
  hydra.java.utils.javaPrimaryToJavaExpression(hydra.java.syntax.Primary.arrayCreation(hydra.java.syntax.ArrayCreationExpression.primitiveArray(hydra.java.syntax.ArrayCreationExpression_PrimitiveArray(primType,
     Seq(), `init_`))))
}

def javaArrayInitializer(exprs: Seq[hydra.java.syntax.Expression]): hydra.java.syntax.ArrayInitializer =
  Seq(hydra.lib.lists.map[hydra.java.syntax.Expression, hydra.java.syntax.VariableInitializer]((e: hydra.java.syntax.Expression) => hydra.java.syntax.VariableInitializer.expression(e))(exprs))

def javaAssignmentStatement(lhs: hydra.java.syntax.LeftHandSide)(rhs: hydra.java.syntax.Expression): hydra.java.syntax.Statement =
  hydra.java.syntax.Statement.withoutTrailing(hydra.java.syntax.StatementWithoutTrailingSubstatement.expression(hydra.java.syntax.StatementExpression.assignment(hydra.java.syntax.Assignment(lhs,
     hydra.java.syntax.AssignmentOperator.simple, rhs))))

def javaBoolean(b: Boolean): hydra.java.syntax.Literal = hydra.java.syntax.Literal.boolean(b)

def javaBooleanExpression(b: Boolean): hydra.java.syntax.Expression =
  hydra.java.utils.javaPrimaryToJavaExpression(hydra.java.utils.javaLiteralToJavaPrimary(hydra.java.utils.javaBoolean(b)))

lazy val javaBooleanType: hydra.java.syntax.Type = hydra.java.utils.javaPrimitiveTypeToJavaType(hydra.java.syntax.PrimitiveType.boolean)

lazy val javaBytePrimitiveType: hydra.java.syntax.PrimitiveTypeWithAnnotations = hydra.java.syntax.PrimitiveTypeWithAnnotations(hydra.java.syntax.PrimitiveType.numeric(hydra.java.syntax.NumericType.integral(hydra.java.syntax.IntegralType.byte)),
   Seq())

def javaCastExpression(rt: hydra.java.syntax.ReferenceType)(expr: hydra.java.syntax.UnaryExpression): hydra.java.syntax.CastExpression =
  hydra.java.syntax.CastExpression.notPlusMinus(hydra.java.syntax.CastExpression_NotPlusMinus(hydra.java.syntax.CastExpression_RefAndBounds(rt,
     Seq()), expr))

def javaCastExpressionToJavaExpression(ce: hydra.java.syntax.CastExpression): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.cast(ce)))))))))))))))

def javaCastPrimitive(pt: hydra.java.syntax.PrimitiveType)(expr: hydra.java.syntax.UnaryExpression): hydra.java.syntax.CastExpression =
  hydra.java.syntax.CastExpression.primitive(hydra.java.syntax.CastExpression_Primitive(hydra.java.syntax.PrimitiveTypeWithAnnotations(pt,
     Seq()), expr))

def javaClassDeclaration(aliases: hydra.java.environment.Aliases)(tparams: Seq[hydra.java.syntax.TypeParameter])(elName: hydra.core.Name)(mods: Seq[hydra.java.syntax.ClassModifier])(supname: Option[hydra.core.Name])(impls: Seq[hydra.java.syntax.InterfaceType])(bodyDecls: Seq[hydra.java.syntax.ClassBodyDeclarationWithComments]): hydra.java.syntax.ClassDeclaration =
  {
  lazy val `extends_`: Option[hydra.java.syntax.ClassType] = hydra.lib.maybes.map[hydra.core.Name,
     hydra.java.syntax.ClassType]((n: hydra.core.Name) =>
    hydra.java.utils.nameToJavaClassType(aliases)(true)(Seq())(n)(None))(supname)
  hydra.java.syntax.ClassDeclaration.normal(hydra.java.syntax.NormalClassDeclaration(mods,
     hydra.java.utils.javaDeclName(elName), tparams, `extends_`, impls, bodyDecls))
}

def javaClassType(args: Seq[hydra.java.syntax.ReferenceType])(pkg: Option[hydra.java.syntax.PackageName])(id: scala.Predef.String): hydra.java.syntax.ClassType =
  {
  lazy val qual: hydra.java.syntax.ClassTypeQualifier = hydra.lib.maybes.cases[hydra.java.syntax.PackageName,
     hydra.java.syntax.ClassTypeQualifier](pkg)(hydra.java.syntax.ClassTypeQualifier.none)((p: hydra.java.syntax.PackageName) => hydra.java.syntax.ClassTypeQualifier.`package`(p))
  lazy val targs: Seq[hydra.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.java.syntax.ReferenceType,
     hydra.java.syntax.TypeArgument]((rt: hydra.java.syntax.ReferenceType) => hydra.java.syntax.TypeArgument.reference(rt))(args)
  hydra.java.syntax.ClassType(Seq(), qual, hydra.java.utils.javaTypeIdentifier(id), targs)
}

def javaClassTypeToJavaType(ct: hydra.java.syntax.ClassType): hydra.java.syntax.Type =
  hydra.java.syntax.Type.reference(hydra.java.syntax.ReferenceType.classOrInterface(hydra.java.syntax.ClassOrInterfaceType.`class`(ct)))

def javaConditionalAndExpressionToJavaExpression(cae: hydra.java.syntax.ConditionalAndExpression): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(cae))))

def javaConstructorCall(ci: hydra.java.syntax.ClassOrInterfaceTypeToInstantiate)(args: Seq[hydra.java.syntax.Expression])(mbody: Option[hydra.java.syntax.ClassBody]): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.classInstance(hydra.java.syntax.ClassInstanceCreationExpression(None,
     hydra.java.syntax.UnqualifiedClassInstanceCreationExpression(Seq(), ci, args,
     mbody))))))))))))))))))))

def javaConstructorName(id: hydra.java.syntax.Identifier)(targs: Option[hydra.java.syntax.TypeArgumentsOrDiamond]): hydra.java.syntax.ClassOrInterfaceTypeToInstantiate =
  hydra.java.syntax.ClassOrInterfaceTypeToInstantiate(Seq(hydra.java.syntax.AnnotatedIdentifier(Seq(), id)), targs)

def javaDeclName(name: hydra.core.Name): hydra.java.syntax.TypeIdentifier = hydra.java.utils.javaVariableName(name)

def javaDoubleCastExpression(rawRt: hydra.java.syntax.ReferenceType)(targetRt: hydra.java.syntax.ReferenceType)(expr: hydra.java.syntax.UnaryExpression): hydra.java.syntax.CastExpression =
  {
  lazy val firstCast: hydra.java.syntax.Expression = hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaCastExpression(rawRt)(expr))
  hydra.java.utils.javaCastExpression(targetRt)(hydra.java.utils.javaExpressionToJavaUnaryExpression(firstCast))
}

def javaDoubleCastExpressionToJavaExpression(rawRt: hydra.java.syntax.ReferenceType)(targetRt: hydra.java.syntax.ReferenceType)(expr: hydra.java.syntax.UnaryExpression): hydra.java.syntax.Expression =
  hydra.java.utils.javaCastExpressionToJavaExpression(hydra.java.utils.javaDoubleCastExpression(rawRt)(targetRt)(expr))

lazy val javaEmptyStatement: hydra.java.syntax.Statement = hydra.java.syntax.Statement.withoutTrailing(hydra.java.syntax.StatementWithoutTrailingSubstatement.empty)

def javaEqualityExpressionToJavaExpression(ee: hydra.java.syntax.EqualityExpression): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(ee))))))))

def javaEqualityExpressionToJavaInclusiveOrExpression(ee: hydra.java.syntax.EqualityExpression): hydra.java.syntax.InclusiveOrExpression = Seq(Seq(Seq(ee)))

def javaEquals(lhs: hydra.java.syntax.EqualityExpression)(rhs: hydra.java.syntax.RelationalExpression): hydra.java.syntax.EqualityExpression =
  hydra.java.syntax.EqualityExpression.equal(hydra.java.syntax.EqualityExpression_Binary(lhs, rhs))

def javaEqualsNull(lhs: hydra.java.syntax.EqualityExpression): hydra.java.syntax.EqualityExpression =
  hydra.java.utils.javaEquals(lhs)(hydra.java.utils.javaLiteralToJavaRelationalExpression(hydra.java.syntax.Literal.`null`))

def javaExpressionNameToJavaExpression(en: hydra.java.syntax.ExpressionName): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.name(en))))))))))))))))

def javaExpressionToJavaPrimary(e: hydra.java.syntax.Expression): hydra.java.syntax.Primary =
  {
  lazy val fallback: hydra.java.syntax.Primary = hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.parens(e))
  e match
    case hydra.java.syntax.Expression.assignment(v_Expression_assignment_ae) => v_Expression_assignment_ae match
      case hydra.java.syntax.AssignmentExpression.conditional(v_AssignmentExpression_conditional_ce) => v_AssignmentExpression_conditional_ce match
        case hydra.java.syntax.ConditionalExpression.simple(v_ConditionalExpression_simple_cor) => {
          lazy val cands: Seq[hydra.java.syntax.ConditionalAndExpression] = v_ConditionalExpression_simple_cor
          hydra.lib.logic.ifElse[hydra.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.ConditionalAndExpression](cands))(1))({
            lazy val iors: Seq[hydra.java.syntax.InclusiveOrExpression] = hydra.lib.lists.head[hydra.java.syntax.ConditionalAndExpression](cands)
            hydra.lib.logic.ifElse[hydra.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.InclusiveOrExpression](iors))(1))({
              lazy val xors: Seq[hydra.java.syntax.ExclusiveOrExpression] = hydra.lib.lists.head[hydra.java.syntax.InclusiveOrExpression](iors)
              hydra.lib.logic.ifElse[hydra.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.ExclusiveOrExpression](xors))(1))({
                lazy val ands: Seq[hydra.java.syntax.AndExpression] = hydra.lib.lists.head[hydra.java.syntax.ExclusiveOrExpression](xors)
                hydra.lib.logic.ifElse[hydra.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.AndExpression](ands))(1))({
                  lazy val eqs: Seq[hydra.java.syntax.EqualityExpression] = hydra.lib.lists.head[hydra.java.syntax.AndExpression](ands)
                  hydra.lib.logic.ifElse[hydra.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.java.syntax.EqualityExpression](eqs))(1))(hydra.lib.lists.head[hydra.java.syntax.EqualityExpression](eqs) match
                    case hydra.java.syntax.EqualityExpression.unary(v_EqualityExpression_unary_rel) => v_EqualityExpression_unary_rel match
                      case hydra.java.syntax.RelationalExpression.simple(v_RelationalExpression_simple_shift) => v_RelationalExpression_simple_shift match
                        case hydra.java.syntax.ShiftExpression.unary(v_ShiftExpression_unary_add) => v_ShiftExpression_unary_add match
                          case hydra.java.syntax.AdditiveExpression.unary(v_AdditiveExpression_unary_mul) => v_AdditiveExpression_unary_mul match
                            case hydra.java.syntax.MultiplicativeExpression.unary(v_MultiplicativeExpression_unary_unary) => v_MultiplicativeExpression_unary_unary match
                              case hydra.java.syntax.UnaryExpression.other(v_UnaryExpression_other_npm) => v_UnaryExpression_other_npm match
                                case hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(v_UnaryExpressionNotPlusMinus_postfix_pf) => v_UnaryExpressionNotPlusMinus_postfix_pf match
                                  case hydra.java.syntax.PostfixExpression.primary(v_PostfixExpression_primary_p) => v_PostfixExpression_primary_p
                                  case _ => fallback
                                case _ => fallback
                              case _ => fallback
                            case _ => fallback
                          case _ => fallback
                        case _ => fallback
                      case _ => fallback
                    case _ => fallback)(fallback)
                })(fallback)
              })(fallback)
            })(fallback)
          })(fallback)
        }
        case _ => fallback
      case _ => fallback
    case _ => fallback
}

def javaExpressionToJavaUnaryExpression(e: hydra.java.syntax.Expression): hydra.java.syntax.UnaryExpression =
  hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.parens(e)))))

def javaFieldAccessToJavaExpression(fa: hydra.java.syntax.FieldAccess): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.fieldAccess(fa))))))))))))))))))

def javaIdentifier(s: scala.Predef.String): hydra.java.syntax.Identifier = hydra.java.utils.sanitizeJavaName(s)

def javaIdentifierToJavaExpression(id: hydra.java.syntax.Identifier): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.name(hydra.java.syntax.ExpressionName(None,
     id)))))))))))))))))

def javaIdentifierToJavaExpressionName(id: hydra.java.syntax.Identifier): hydra.java.syntax.ExpressionName = hydra.java.syntax.ExpressionName(None,
   id)

def javaIdentifierToJavaRelationalExpression(id: hydra.java.syntax.Identifier): hydra.java.syntax.RelationalExpression =
  hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.name(hydra.java.syntax.ExpressionName(None,
     id))))))))

def javaIdentifierToJavaUnaryExpression(id: hydra.java.syntax.Identifier): hydra.java.syntax.UnaryExpression =
  hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.name(hydra.java.syntax.ExpressionName(None,
     id))))

def javaInstanceOf(lhs: hydra.java.syntax.RelationalExpression)(rhs: hydra.java.syntax.ReferenceType): hydra.java.syntax.RelationalExpression =
  hydra.java.syntax.RelationalExpression.instanceof(hydra.java.syntax.RelationalExpression_InstanceOf(lhs, rhs))

def javaInt(i: BigInt): hydra.java.syntax.Literal = hydra.java.syntax.Literal.integer(i)

def javaIntExpression(i: BigInt): hydra.java.syntax.Expression =
  hydra.java.utils.javaPrimaryToJavaExpression(hydra.java.utils.javaLiteralToJavaPrimary(hydra.java.utils.javaInt(i)))

lazy val javaIntType: hydra.java.syntax.Type = hydra.java.utils.javaPrimitiveTypeToJavaType(hydra.java.syntax.PrimitiveType.numeric(hydra.java.syntax.NumericType.integral(hydra.java.syntax.IntegralType.int)))

def javaInterfaceDeclarationToJavaClassBodyDeclaration(nid: hydra.java.syntax.NormalInterfaceDeclaration): hydra.java.syntax.ClassBodyDeclaration =
  hydra.java.syntax.ClassBodyDeclaration.classMember(hydra.java.syntax.ClassMemberDeclaration.interface(hydra.java.syntax.InterfaceDeclaration.normalInterface(nid)))

def javaLambda(v: hydra.core.Name)(body: hydra.java.syntax.Expression): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.lambda(hydra.java.syntax.LambdaExpression(hydra.java.syntax.LambdaParameters.single(hydra.java.utils.variableToJavaIdentifier(v)),
     hydra.java.syntax.LambdaBody.expression(body)))

def javaLambdaFromBlock(v: hydra.core.Name)(block: hydra.java.syntax.Block): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.lambda(hydra.java.syntax.LambdaExpression(hydra.java.syntax.LambdaParameters.single(hydra.java.utils.variableToJavaIdentifier(v)),
     hydra.java.syntax.LambdaBody.block(block)))

def javaLiteralToJavaExpression(lit: hydra.java.syntax.Literal): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.literal(lit))))))))))))))))))

def javaLiteralToJavaMultiplicativeExpression(lit: hydra.java.syntax.Literal): hydra.java.syntax.MultiplicativeExpression =
  hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.literal(lit))))))

def javaLiteralToJavaPrimary(lit: hydra.java.syntax.Literal): hydra.java.syntax.Primary =
  hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.literal(lit))

def javaLiteralToJavaRelationalExpression(lit: hydra.java.syntax.Literal): hydra.java.syntax.RelationalExpression =
  hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.literal(lit)))))))))

def javaMemberField(mods: Seq[hydra.java.syntax.FieldModifier])(jt: hydra.java.syntax.Type)(v: hydra.java.syntax.VariableDeclarator): hydra.java.syntax.ClassBodyDeclaration =
  hydra.java.syntax.ClassBodyDeclaration.classMember(hydra.java.syntax.ClassMemberDeclaration.field(hydra.java.syntax.FieldDeclaration(mods,
     jt, Seq(v))))

def javaMethodBody(mstmts: Option[Seq[hydra.java.syntax.BlockStatement]]): hydra.java.syntax.MethodBody =
  hydra.lib.maybes.cases[Seq[hydra.java.syntax.BlockStatement], hydra.java.syntax.MethodBody](mstmts)(hydra.java.syntax.MethodBody.none)((stmts: Seq[hydra.java.syntax.BlockStatement]) => hydra.java.syntax.MethodBody.block(stmts))

def javaMethodDeclarationToJavaClassBodyDeclaration(md: hydra.java.syntax.MethodDeclaration): hydra.java.syntax.ClassBodyDeclaration =
  hydra.java.syntax.ClassBodyDeclaration.classMember(hydra.java.syntax.ClassMemberDeclaration.method(md))

def javaMethodHeader(tparams: Seq[hydra.java.syntax.TypeParameter])(methodName: scala.Predef.String)(params: Seq[hydra.java.syntax.FormalParameter])(result: hydra.java.syntax.Result): hydra.java.syntax.MethodHeader =
  hydra.java.syntax.MethodHeader(tparams, result, hydra.java.syntax.MethodDeclarator(methodName, None, params), None)

def javaMethodInvocationToJavaExpression(mi: hydra.java.syntax.MethodInvocation): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.methodInvocation(mi))))))))))))))))))

def javaMethodInvocationToJavaPostfixExpression(mi: hydra.java.syntax.MethodInvocation): hydra.java.syntax.PostfixExpression =
  hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.methodInvocation(mi)))

def javaMethodInvocationToJavaPrimary(mi: hydra.java.syntax.MethodInvocation): hydra.java.syntax.Primary =
  hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.methodInvocation(mi))

def javaMethodInvocationToJavaStatement(mi: hydra.java.syntax.MethodInvocation): hydra.java.syntax.Statement =
  hydra.java.syntax.Statement.withoutTrailing(hydra.java.syntax.StatementWithoutTrailingSubstatement.expression(hydra.java.syntax.StatementExpression.methodInvocation(mi)))

def javaMultiplicativeExpressionToJavaRelationalExpression(me: hydra.java.syntax.MultiplicativeExpression): hydra.java.syntax.RelationalExpression =
  hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(me)))

def javaPackageDeclaration(ns: hydra.packaging.Namespace): hydra.java.syntax.PackageDeclaration =
  hydra.java.syntax.PackageDeclaration(Seq(), hydra.lib.lists.map[scala.Predef.String,
     hydra.java.syntax.Identifier]((s: scala.Predef.String) => s)(hydra.lib.strings.splitOn(".")(ns)))

def javaPostfixExpressionToJavaEqualityExpression(pe: hydra.java.syntax.PostfixExpression): hydra.java.syntax.EqualityExpression =
  hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe)))))))

def javaPostfixExpressionToJavaExpression(pe: hydra.java.syntax.PostfixExpression): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe)))))))))))))))

def javaPostfixExpressionToJavaInclusiveOrExpression(pe: hydra.java.syntax.PostfixExpression): hydra.java.syntax.InclusiveOrExpression =
  Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe))))))))))

def javaPostfixExpressionToJavaRelationalExpression(pe: hydra.java.syntax.PostfixExpression): hydra.java.syntax.RelationalExpression =
  hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe))))))

def javaPostfixExpressionToJavaUnaryExpression(pe: hydra.java.syntax.PostfixExpression): hydra.java.syntax.UnaryExpression =
  hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe))

def javaPrimaryToJavaExpression(p: hydra.java.syntax.Primary): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(p))))))))))))))))

def javaPrimaryToJavaUnaryExpression(p: hydra.java.syntax.Primary): hydra.java.syntax.UnaryExpression =
  hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(p)))

def javaPrimitiveTypeToJavaType(pt: hydra.java.syntax.PrimitiveType): hydra.java.syntax.Type =
  hydra.java.syntax.Type.primitive(hydra.java.syntax.PrimitiveTypeWithAnnotations(pt, Seq()))

def javaRefType(args: Seq[hydra.java.syntax.ReferenceType])(pkg: Option[hydra.java.syntax.PackageName])(id: scala.Predef.String): hydra.java.syntax.Type =
  hydra.java.syntax.Type.reference(hydra.java.syntax.ReferenceType.classOrInterface(hydra.java.syntax.ClassOrInterfaceType.`class`(hydra.java.utils.javaClassType(args)(pkg)(id))))

def javaReferenceTypeToRawType(rt: hydra.java.syntax.ReferenceType): hydra.java.syntax.ReferenceType =
  rt match
  case hydra.java.syntax.ReferenceType.classOrInterface(v_ReferenceType_classOrInterface_cit) => v_ReferenceType_classOrInterface_cit match
    case hydra.java.syntax.ClassOrInterfaceType.`class`(v_ClassOrInterfaceType_class_ct) => {
      lazy val anns: Seq[hydra.java.syntax.Annotation] = (v_ClassOrInterfaceType_class_ct.annotations)
      lazy val qual: hydra.java.syntax.ClassTypeQualifier = (v_ClassOrInterfaceType_class_ct.qualifier)
      lazy val id: hydra.java.syntax.TypeIdentifier = (v_ClassOrInterfaceType_class_ct.identifier)
      hydra.java.syntax.ReferenceType.classOrInterface(hydra.java.syntax.ClassOrInterfaceType.`class`(hydra.java.syntax.ClassType(anns,
         qual, id, Seq())))
    }
    case hydra.java.syntax.ClassOrInterfaceType.interface(v_ClassOrInterfaceType_interface_it) => {
      lazy val ct: hydra.java.syntax.ClassType = v_ClassOrInterfaceType_interface_it
      lazy val anns: Seq[hydra.java.syntax.Annotation] = (ct.annotations)
      lazy val qual: hydra.java.syntax.ClassTypeQualifier = (ct.qualifier)
      lazy val id: hydra.java.syntax.TypeIdentifier = (ct.identifier)
      hydra.java.syntax.ReferenceType.classOrInterface(hydra.java.syntax.ClassOrInterfaceType.interface(hydra.java.syntax.ClassType(anns,
         qual, id, Seq())))
    }
  case _ => rt

def javaRelationalExpressionToJavaEqualityExpression(re: hydra.java.syntax.RelationalExpression): hydra.java.syntax.EqualityExpression = hydra.java.syntax.EqualityExpression.unary(re)

def javaRelationalExpressionToJavaExpression(re: hydra.java.syntax.RelationalExpression): hydra.java.syntax.Expression =
  hydra.java.utils.javaEqualityExpressionToJavaExpression(hydra.java.syntax.EqualityExpression.unary(re))

def javaRelationalExpressionToJavaUnaryExpression(re: hydra.java.syntax.RelationalExpression): hydra.java.syntax.UnaryExpression =
  hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.parens(hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(re))))))))))))))

def javaReturnStatement(mex: Option[hydra.java.syntax.Expression]): hydra.java.syntax.Statement =
  hydra.java.syntax.Statement.withoutTrailing(hydra.java.syntax.StatementWithoutTrailingSubstatement.`return`(mex))

def javaStatementsToBlock(stmts: Seq[hydra.java.syntax.Statement]): hydra.java.syntax.Block =
  hydra.lib.lists.map[hydra.java.syntax.Statement, hydra.java.syntax.BlockStatement]((s: hydra.java.syntax.Statement) => hydra.java.syntax.BlockStatement.statement(s))(stmts)

def javaString(s: scala.Predef.String): hydra.java.syntax.Literal = hydra.java.syntax.Literal.string(s)

def javaStringMultiplicativeExpression(s: scala.Predef.String): hydra.java.syntax.MultiplicativeExpression =
  hydra.java.utils.javaLiteralToJavaMultiplicativeExpression(hydra.java.utils.javaString(s))

lazy val javaThis: hydra.java.syntax.Expression = hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(hydra.java.syntax.UnaryExpression.other(hydra.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.java.syntax.PostfixExpression.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.`this`)))))))))))))))))

def javaThrowIllegalArgumentException(args: Seq[hydra.java.syntax.Expression]): hydra.java.syntax.Statement =
  hydra.java.utils.javaThrowStatement(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("IllegalArgumentException")(None))(args)(None))

def javaThrowIllegalStateException(args: Seq[hydra.java.syntax.Expression]): hydra.java.syntax.Statement =
  hydra.java.utils.javaThrowStatement(hydra.java.utils.javaConstructorCall(hydra.java.utils.javaConstructorName("IllegalStateException")(None))(args)(None))

def javaThrowStatement(e: hydra.java.syntax.Expression): hydra.java.syntax.Statement =
  hydra.java.syntax.Statement.withoutTrailing(hydra.java.syntax.StatementWithoutTrailingSubstatement.`throw`(e))

def javaTypeFromTypeName(aliases: hydra.java.environment.Aliases)(elName: hydra.core.Name): hydra.java.syntax.Type =
  hydra.java.utils.javaTypeVariableToType(hydra.java.syntax.TypeVariable(Seq(), hydra.java.utils.nameToJavaTypeIdentifier(aliases)(false)(elName)))

def javaTypeIdentifier(s: scala.Predef.String): hydra.java.syntax.TypeIdentifier = s

def javaTypeIdentifierToJavaTypeArgument(id: hydra.java.syntax.TypeIdentifier): hydra.java.syntax.TypeArgument =
  hydra.java.syntax.TypeArgument.reference(hydra.java.syntax.ReferenceType.variable(hydra.java.syntax.TypeVariable(Seq(),
     id)))

def javaTypeName(id: hydra.java.syntax.Identifier): hydra.java.syntax.TypeName = hydra.java.syntax.TypeName(id, None)

def javaTypeParameter(v: scala.Predef.String): hydra.java.syntax.TypeParameter =
  hydra.java.syntax.TypeParameter(Seq(), hydra.java.utils.javaTypeIdentifier(v), None)

def javaTypeToJavaFormalParameter(jt: hydra.java.syntax.Type)(fname: hydra.core.Name): hydra.java.syntax.FormalParameter =
  hydra.java.syntax.FormalParameter.simple(hydra.java.syntax.FormalParameter_Simple(Seq(),
     jt, hydra.java.utils.fieldNameToJavaVariableDeclaratorId(fname)))

def javaTypeToJavaReferenceType[T0](t: hydra.java.syntax.Type)(cx: T0): Either[hydra.errors.Error,
   hydra.java.syntax.ReferenceType] =
  t match
  case hydra.java.syntax.Type.reference(v_Type_reference_rt) => Right(v_Type_reference_rt)
  case hydra.java.syntax.Type.primitive(v_Type_primitive__) => Left(hydra.errors.Error.other("expected a Java reference type"))

def javaTypeToJavaResult(jt: hydra.java.syntax.Type): hydra.java.syntax.Result = hydra.java.syntax.Result.`type`(jt)

def javaTypeToJavaTypeArgument(t: hydra.java.syntax.Type): hydra.java.syntax.TypeArgument =
  t match
  case hydra.java.syntax.Type.reference(v_Type_reference_rt) => hydra.java.syntax.TypeArgument.reference(v_Type_reference_rt)
  case hydra.java.syntax.Type.primitive(v_Type_primitive__) => hydra.java.syntax.TypeArgument.wildcard(hydra.java.syntax.Wildcard(Seq(),
     None))

def javaTypeVariable(v: scala.Predef.String): hydra.java.syntax.ReferenceType =
  hydra.java.syntax.ReferenceType.variable(hydra.java.syntax.TypeVariable(Seq(), hydra.java.utils.javaTypeIdentifier(hydra.formatting.capitalize(v))))

def javaTypeVariableToType(tv: hydra.java.syntax.TypeVariable): hydra.java.syntax.Type = hydra.java.syntax.Type.reference(hydra.java.syntax.ReferenceType.variable(tv))

def javaUnaryExpressionToJavaExpression(ue: hydra.java.syntax.UnaryExpression): hydra.java.syntax.Expression =
  hydra.java.syntax.Expression.assignment(hydra.java.syntax.AssignmentExpression.conditional(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.java.syntax.EqualityExpression.unary(hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(ue)))))))))))))

def javaUnaryExpressionToJavaRelationalExpression(ue: hydra.java.syntax.UnaryExpression): hydra.java.syntax.RelationalExpression =
  hydra.java.syntax.RelationalExpression.simple(hydra.java.syntax.ShiftExpression.unary(hydra.java.syntax.AdditiveExpression.unary(hydra.java.syntax.MultiplicativeExpression.unary(ue))))

def javaVariableDeclarator(id: hydra.java.syntax.Identifier)(minit: Option[hydra.java.syntax.VariableInitializer]): hydra.java.syntax.VariableDeclarator =
  hydra.java.syntax.VariableDeclarator(hydra.java.utils.javaVariableDeclaratorId(id), minit)

def javaVariableDeclaratorId(id: hydra.java.syntax.Identifier): hydra.java.syntax.VariableDeclaratorId = hydra.java.syntax.VariableDeclaratorId(id,
   None)

def javaVariableName(name: hydra.core.Name): hydra.java.syntax.Identifier = hydra.java.utils.javaIdentifier(hydra.names.localNameOf(name))

def lookupJavaVarName(aliases: hydra.java.environment.Aliases)(name: hydra.core.Name): hydra.core.Name =
  hydra.lib.maybes.cases[hydra.core.Name, hydra.core.Name](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.Name](name)(aliases.varRenames))(name)((renamed: hydra.core.Name) => renamed)

def makeConstructor(aliases: hydra.java.environment.Aliases)(elName: hydra.core.Name)(`private`: Boolean)(params: Seq[hydra.java.syntax.FormalParameter])(stmts: Seq[hydra.java.syntax.BlockStatement]): hydra.java.syntax.ClassBodyDeclaration =
  {
  lazy val nm: hydra.java.syntax.SimpleTypeName = hydra.java.utils.nameToJavaTypeIdentifier(aliases)(false)(elName)
  lazy val cons: hydra.java.syntax.ConstructorDeclarator = hydra.java.syntax.ConstructorDeclarator(Seq(),
     nm, None, params)
  lazy val mods: Seq[hydra.java.syntax.ConstructorModifier] = Seq(hydra.lib.logic.ifElse[hydra.java.syntax.ConstructorModifier](`private`)(hydra.java.syntax.ConstructorModifier.`private`)(hydra.java.syntax.ConstructorModifier.public))
  lazy val body: hydra.java.syntax.ConstructorBody = hydra.java.syntax.ConstructorBody(None, stmts)
  hydra.java.syntax.ClassBodyDeclaration.constructorDeclaration(hydra.java.syntax.ConstructorDeclaration(mods,
     cons, None, body))
}

def methodDeclaration(mods: Seq[hydra.java.syntax.MethodModifier])(tparams: Seq[hydra.java.syntax.TypeParameter])(anns: Seq[hydra.java.syntax.Annotation])(methodName: scala.Predef.String)(params: Seq[hydra.java.syntax.FormalParameter])(result: hydra.java.syntax.Result)(stmts: Option[Seq[hydra.java.syntax.BlockStatement]]): hydra.java.syntax.ClassBodyDeclaration =
  hydra.java.utils.javaMethodDeclarationToJavaClassBodyDeclaration(hydra.java.syntax.MethodDeclaration(anns,
     mods, hydra.java.utils.javaMethodHeader(tparams)(methodName)(params)(result),
     hydra.java.utils.javaMethodBody(stmts)))

def methodInvocation(lhs: Option[Either[hydra.java.syntax.ExpressionName, hydra.java.syntax.Primary]])(methodName: hydra.java.syntax.Identifier)(args: Seq[hydra.java.syntax.Expression]): hydra.java.syntax.MethodInvocation =
  {
  lazy val header: hydra.java.syntax.MethodInvocation_Header = hydra.lib.maybes.cases[Either[hydra.java.syntax.ExpressionName,
     hydra.java.syntax.Primary], hydra.java.syntax.MethodInvocation_Header](lhs)(hydra.java.syntax.MethodInvocation_Header.simple(methodName))((either: Either[hydra.java.syntax.ExpressionName,
     hydra.java.syntax.Primary]) =>
    hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.lib.eithers.either[hydra.java.syntax.ExpressionName,
       hydra.java.syntax.Primary, hydra.java.syntax.MethodInvocation_Variant]((en: hydra.java.syntax.ExpressionName) => hydra.java.syntax.MethodInvocation_Variant.expression(en))((p: hydra.java.syntax.Primary) => hydra.java.syntax.MethodInvocation_Variant.primary(p))(either),
       Seq(), methodName)))
  hydra.java.syntax.MethodInvocation(header, args)
}

def methodInvocationStatic(self: hydra.java.syntax.Identifier)(methodName: hydra.java.syntax.Identifier)(args: Seq[hydra.java.syntax.Expression]): hydra.java.syntax.MethodInvocation =
  hydra.java.utils.methodInvocation(Some(Left(hydra.java.utils.javaIdentifierToJavaExpressionName(self))))(methodName)(args)

def methodInvocationStaticWithTypeArgs(self: hydra.java.syntax.Identifier)(methodName: hydra.java.syntax.Identifier)(targs: Seq[hydra.java.syntax.TypeArgument])(args: Seq[hydra.java.syntax.Expression]): hydra.java.syntax.MethodInvocation =
  {
  lazy val header: hydra.java.syntax.MethodInvocation_Header = hydra.java.syntax.MethodInvocation_Header.complex(hydra.java.syntax.MethodInvocation_Complex(hydra.java.syntax.MethodInvocation_Variant.expression(hydra.java.utils.javaIdentifierToJavaExpressionName(self)),
     targs, methodName))
  hydra.java.syntax.MethodInvocation(header, args)
}

def nameToJavaClassType(aliases: hydra.java.environment.Aliases)(qualify: Boolean)(args: Seq[hydra.java.syntax.TypeArgument])(name: hydra.core.Name)(mlocal: Option[scala.Predef.String]): hydra.java.syntax.ClassType =
  {
  lazy val result: Tuple2[hydra.java.syntax.TypeIdentifier, hydra.java.syntax.ClassTypeQualifier] = hydra.java.utils.nameToQualifiedJavaName(aliases)(qualify)(name)(mlocal)
  lazy val id: hydra.java.syntax.TypeIdentifier = hydra.lib.pairs.first[hydra.java.syntax.TypeIdentifier,
     hydra.java.syntax.ClassTypeQualifier](result)
  lazy val pkg: hydra.java.syntax.ClassTypeQualifier = hydra.lib.pairs.second[hydra.java.syntax.TypeIdentifier,
     hydra.java.syntax.ClassTypeQualifier](result)
  hydra.java.syntax.ClassType(Seq(), pkg, id, args)
}

def nameToJavaName(aliases: hydra.java.environment.Aliases)(name: hydra.core.Name): hydra.java.syntax.Identifier =
  {
  lazy val qn: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
  lazy val `ns_`: Option[hydra.packaging.Namespace] = (qn.namespace)
  lazy val local: scala.Predef.String = (qn.local)
  hydra.lib.logic.ifElse[hydra.java.syntax.Identifier](hydra.java.utils.isEscaped(name))(hydra.java.utils.sanitizeJavaName(local))(hydra.lib.maybes.cases[hydra.packaging.Namespace,
     hydra.java.syntax.Identifier](`ns_`)(local)((gname: hydra.packaging.Namespace) =>
    {
    lazy val parts: Seq[scala.Predef.String] = hydra.lib.maybes.cases[hydra.java.syntax.PackageName,
       Seq[scala.Predef.String]](hydra.lib.maps.lookup[hydra.packaging.Namespace,
       hydra.java.syntax.PackageName](gname)(aliases.packages))(hydra.lib.strings.splitOn(".")(gname))((pkgName: hydra.java.syntax.PackageName) =>
      hydra.lib.lists.map[hydra.java.syntax.Identifier, scala.Predef.String]((i: hydra.java.syntax.Identifier) => i)(pkgName))
    lazy val allParts: Seq[scala.Predef.String] = hydra.lib.lists.concat2[scala.Predef.String](parts)(Seq(hydra.java.utils.sanitizeJavaName(local)))
    hydra.lib.strings.intercalate(".")(allParts)
  }))
}

def nameToJavaReferenceType(aliases: hydra.java.environment.Aliases)(qualify: Boolean)(args: Seq[hydra.java.syntax.TypeArgument])(name: hydra.core.Name)(mlocal: Option[scala.Predef.String]): hydra.java.syntax.ReferenceType =
  hydra.java.syntax.ReferenceType.classOrInterface(hydra.java.syntax.ClassOrInterfaceType.`class`(hydra.java.utils.nameToJavaClassType(aliases)(qualify)(args)(name)(mlocal)))

def nameToJavaTypeIdentifier(aliases: hydra.java.environment.Aliases)(qualify: Boolean)(name: hydra.core.Name): hydra.java.syntax.TypeIdentifier =
  hydra.lib.pairs.first[hydra.java.syntax.TypeIdentifier, hydra.java.syntax.ClassTypeQualifier](hydra.java.utils.nameToQualifiedJavaName(aliases)(qualify)(name)(None))

def nameToQualifiedJavaName(aliases: hydra.java.environment.Aliases)(qualify: Boolean)(name: hydra.core.Name)(mlocal: Option[scala.Predef.String]): Tuple2[hydra.java.syntax.TypeIdentifier,
   hydra.java.syntax.ClassTypeQualifier] =
  {
  lazy val qn: hydra.packaging.QualifiedName = hydra.names.qualifyName(name)
  lazy val `ns_`: Option[hydra.packaging.Namespace] = (qn.namespace)
  lazy val local: scala.Predef.String = (qn.local)
  lazy val alias: Option[hydra.java.syntax.PackageName] = hydra.lib.maybes.cases[hydra.packaging.Namespace,
     Option[hydra.java.syntax.PackageName]](`ns_`)(None)((n: hydra.packaging.Namespace) =>
    Some(hydra.lib.maybes.cases[hydra.java.syntax.PackageName, hydra.java.syntax.PackageName](hydra.lib.maps.lookup[hydra.packaging.Namespace,
       hydra.java.syntax.PackageName](n)(aliases.packages))(hydra.java.names.javaPackageName(hydra.lib.strings.splitOn(".")(n)))((id: hydra.java.syntax.PackageName) => id)))
  lazy val pkg: hydra.java.syntax.ClassTypeQualifier = hydra.lib.logic.ifElse[hydra.java.syntax.ClassTypeQualifier](qualify)(hydra.lib.maybes.cases[hydra.java.syntax.PackageName,
     hydra.java.syntax.ClassTypeQualifier](alias)(hydra.java.syntax.ClassTypeQualifier.none)((p: hydra.java.syntax.PackageName) => hydra.java.syntax.ClassTypeQualifier.`package`(p)))(hydra.java.syntax.ClassTypeQualifier.none)
  lazy val jid: hydra.java.syntax.TypeIdentifier = hydra.java.utils.javaTypeIdentifier(hydra.lib.maybes.cases[scala.Predef.String,
     scala.Predef.String](mlocal)(hydra.java.utils.sanitizeJavaName(local))((l: scala.Predef.String) =>
    hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.java.utils.sanitizeJavaName(local))("."))(hydra.java.utils.sanitizeJavaName(l))))
  Tuple2(jid, pkg)
}

lazy val overrideAnnotation: hydra.java.syntax.Annotation = hydra.java.syntax.Annotation.marker(hydra.java.utils.javaTypeName("Override"))

def referenceTypeToResult(rt: hydra.java.syntax.ReferenceType): hydra.java.syntax.Result = hydra.java.utils.javaTypeToJavaResult(hydra.java.syntax.Type.reference(rt))

def sanitizeJavaName(name: scala.Predef.String): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.java.utils.isEscaped(name))(hydra.java.utils.unescape(name))(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](name)("_"))("ignored")(hydra.formatting.sanitizeWithUnderscores(hydra.java.language.reservedWords)(name)))

lazy val suppressWarningsUncheckedAnnotation: hydra.java.syntax.Annotation = hydra.java.syntax.Annotation.singleElement(hydra.java.syntax.SingleElementAnnotation(hydra.java.utils.javaTypeName("SuppressWarnings"),
   Some(hydra.java.syntax.ElementValue.conditionalExpression(hydra.java.syntax.ConditionalExpression.simple(Seq(Seq(hydra.java.utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.java.syntax.PostfixExpression.primary(hydra.java.utils.javaLiteralToJavaPrimary(hydra.java.utils.javaString("unchecked")))))))))))

def toAcceptMethod(`abstract`: Boolean)(vtparams: Seq[hydra.java.syntax.TypeParameter]): hydra.java.syntax.ClassBodyDeclaration =
  {
  lazy val mods: Seq[hydra.java.syntax.MethodModifier] = hydra.lib.logic.ifElse[Seq[hydra.java.syntax.MethodModifier]](`abstract`)(Seq(hydra.java.syntax.MethodModifier.public,
     hydra.java.syntax.MethodModifier.`abstract`))(Seq(hydra.java.syntax.MethodModifier.public))
  lazy val tparams: Seq[hydra.java.syntax.TypeParameter] = Seq(hydra.java.utils.javaTypeParameter(hydra.java.names.visitorReturnParameter))
  lazy val anns: Seq[hydra.java.syntax.Annotation] = hydra.lib.logic.ifElse[Seq[hydra.java.syntax.Annotation]](`abstract`)(Seq())(Seq(hydra.java.utils.overrideAnnotation))
  lazy val typeArgs: Seq[hydra.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.java.syntax.TypeParameter,
     hydra.java.syntax.TypeArgument]((tp: hydra.java.syntax.TypeParameter) =>
    hydra.java.syntax.TypeArgument.reference(hydra.java.utils.typeParameterToReferenceType(tp)))(vtparams)
  lazy val ref: hydra.java.syntax.Type = hydra.java.utils.javaClassTypeToJavaType(hydra.java.syntax.ClassType(Seq(),
     hydra.java.syntax.ClassTypeQualifier.none, hydra.java.utils.javaTypeIdentifier(hydra.java.names.visitorName),
     hydra.lib.lists.concat2[hydra.java.syntax.TypeArgument](typeArgs)(Seq(hydra.java.syntax.TypeArgument.reference(hydra.java.utils.visitorTypeVariable)))))
  lazy val param: hydra.java.syntax.FormalParameter = hydra.java.utils.javaTypeToJavaFormalParameter(ref)("visitor")
  lazy val result: hydra.java.syntax.Result = hydra.java.utils.javaTypeToJavaResult(hydra.java.syntax.Type.reference(hydra.java.utils.visitorTypeVariable))
  lazy val returnExpr: hydra.java.syntax.Expression = hydra.java.utils.javaMethodInvocationToJavaExpression(hydra.java.utils.methodInvocationStatic("visitor")(hydra.java.names.visitMethodName)(Seq(hydra.java.utils.javaThis)))
  lazy val body: Option[Seq[hydra.java.syntax.BlockStatement]] = hydra.lib.logic.ifElse[Option[Seq[hydra.java.syntax.BlockStatement]]](`abstract`)(None)(Some(Seq(hydra.java.syntax.BlockStatement.statement(hydra.java.utils.javaReturnStatement(Some(returnExpr))))))
  hydra.java.utils.methodDeclaration(mods)(tparams)(anns)(hydra.java.names.acceptMethodName)(Seq(param))(result)(body)
}

def toAssignStmt(fname: hydra.core.Name): hydra.java.syntax.Statement =
  {
  lazy val id: hydra.java.syntax.Identifier = hydra.java.utils.fieldNameToJavaIdentifier(fname)
  lazy val lhs: hydra.java.syntax.LeftHandSide = hydra.java.syntax.LeftHandSide.fieldAccess(hydra.java.syntax.FieldAccess(hydra.java.syntax.FieldAccess_Qualifier.primary(hydra.java.syntax.Primary.noNewArray(hydra.java.syntax.PrimaryNoNewArrayExpression.`this`)),
     id))
  lazy val rhs: hydra.java.syntax.Expression = hydra.java.utils.fieldNameToJavaExpression(fname)
  hydra.java.utils.javaAssignmentStatement(lhs)(rhs)
}

def toJavaArrayType[T0](t: hydra.java.syntax.Type)(cx: T0): Either[hydra.errors.Error, hydra.java.syntax.Type] =
  t match
  case hydra.java.syntax.Type.reference(v_Type_reference_rt) => v_Type_reference_rt match
    case hydra.java.syntax.ReferenceType.classOrInterface(v_ReferenceType_classOrInterface_cit) => Right(hydra.java.syntax.Type.reference(hydra.java.syntax.ReferenceType.array(hydra.java.syntax.ArrayType(Seq(Seq()),
       hydra.java.syntax.ArrayType_Variant.classOrInterface(v_ReferenceType_classOrInterface_cit)))))
    case hydra.java.syntax.ReferenceType.array(v_ReferenceType_array_at) => {
      lazy val oldDims: Seq[Seq[hydra.java.syntax.Annotation]] = (v_ReferenceType_array_at.dims)
      lazy val newDims: hydra.java.syntax.Dims = hydra.lib.lists.concat2[Seq[hydra.java.syntax.Annotation]](oldDims)(Seq(Seq()))
      lazy val variant: hydra.java.syntax.ArrayType_Variant = (v_ReferenceType_array_at.variant)
      Right(hydra.java.syntax.Type.reference(hydra.java.syntax.ReferenceType.array(hydra.java.syntax.ArrayType(newDims,
         variant))))
    }
    case hydra.java.syntax.ReferenceType.variable(v_ReferenceType_variable__) => Left(hydra.errors.Error.other("don't know how to make Java reference type into array type"))
  case hydra.java.syntax.Type.primitive(v_Type_primitive__) => Left(hydra.errors.Error.other("don't know how to make Java type into array type"))

def typeParameterToReferenceType(tp: hydra.java.syntax.TypeParameter): hydra.java.syntax.ReferenceType = hydra.java.utils.javaTypeVariable(tp.identifier)

def typeParameterToTypeArgument(tp: hydra.java.syntax.TypeParameter): hydra.java.syntax.TypeArgument = hydra.java.utils.javaTypeIdentifierToJavaTypeArgument(tp.identifier)

def unTypeParameter(tp: hydra.java.syntax.TypeParameter): scala.Predef.String = (tp.identifier)

def unescape(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.fromList(hydra.lib.lists.tail[Int](hydra.lib.strings.toList(s)))

def uniqueVarName(aliases: hydra.java.environment.Aliases)(name: hydra.core.Name): hydra.core.Name =
  hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](name)(aliases.inScopeJavaVars))(hydra.java.utils.uniqueVarName_go(aliases)(name)(2))(name)

def uniqueVarName_go(aliases: hydra.java.environment.Aliases)(base: scala.Predef.String)(n: Int): hydra.core.Name =
  {
  lazy val candidate: hydra.core.Name = hydra.lib.strings.cat2(base)(hydra.lib.literals.showInt32(n))
  hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](candidate)(aliases.inScopeJavaVars))(hydra.java.utils.uniqueVarName_go(aliases)(base)(hydra.lib.math.add(n)(1)))(candidate)
}

def varDeclarationStatement(id: hydra.java.syntax.Identifier)(rhs: hydra.java.syntax.Expression): hydra.java.syntax.BlockStatement =
  hydra.java.syntax.BlockStatement.localVariableDeclaration(hydra.java.syntax.LocalVariableDeclaration(Seq(),
     hydra.java.syntax.LocalVariableType.`var`, Seq(hydra.java.utils.javaVariableDeclarator(id)(Some(hydra.java.syntax.VariableInitializer.expression(rhs))))))

def variableDeclarationStatement[T0](aliases: T0)(jtype: hydra.java.syntax.Type)(id: hydra.java.syntax.Identifier)(rhs: hydra.java.syntax.Expression): hydra.java.syntax.BlockStatement =
  {
  lazy val `init_`: hydra.java.syntax.VariableInitializer = hydra.java.syntax.VariableInitializer.expression(rhs)
  lazy val vdec: hydra.java.syntax.VariableDeclarator = hydra.java.utils.javaVariableDeclarator(id)(Some(`init_`))
  hydra.java.syntax.BlockStatement.localVariableDeclaration(hydra.java.syntax.LocalVariableDeclaration(Seq(),
     hydra.java.syntax.LocalVariableType.`type`(jtype), Seq(vdec)))
}

def variableToJavaIdentifier(name: hydra.core.Name): hydra.java.syntax.Identifier =
  {
  lazy val v: scala.Predef.String = name
  hydra.lib.logic.ifElse[hydra.java.syntax.Identifier](hydra.lib.equality.equal[scala.Predef.String](v)("_"))("ignored")(hydra.java.utils.sanitizeJavaName(v))
}

def variantClassName(qualify: Boolean)(elName: hydra.core.Name)(fname: hydra.core.Name): hydra.core.Name =
  {
  lazy val qn: hydra.packaging.QualifiedName = hydra.names.qualifyName(elName)
  lazy val `ns_`: Option[hydra.packaging.Namespace] = (qn.namespace)
  lazy val local: scala.Predef.String = (qn.local)
  lazy val flocal: scala.Predef.String = hydra.formatting.capitalize(fname)
  lazy val local1: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](qualify)(hydra.lib.strings.cat2(hydra.lib.strings.cat2(local)("."))(flocal))(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](flocal)(local))(hydra.lib.strings.cat2(flocal)("_"))(flocal))
  hydra.names.unqualifyName(hydra.packaging.QualifiedName(`ns_`, local1))
}

lazy val visitorTypeVariable: hydra.java.syntax.ReferenceType = hydra.java.utils.javaTypeVariable("r")
