package hydra.ext.java.utils

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.ext.java.environment.*

import hydra.ext.java.syntax.*

import hydra.module.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def javaIdentifier(s: scala.Predef.String): hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.sanitizeJavaName(s)

def javaTypeIdentifier(s: scala.Predef.String): hydra.ext.java.syntax.TypeIdentifier = s

def javaTypeName(id: hydra.ext.java.syntax.Identifier): hydra.ext.java.syntax.TypeName = hydra.ext.java.syntax.TypeName(id, None)

def javaDeclName(name: hydra.core.Name): hydra.ext.java.syntax.TypeIdentifier = hydra.ext.java.utils.javaVariableName(name)

def javaVariableName(name: hydra.core.Name): hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.javaIdentifier(hydra.names.localNameOf(name))

def javaVariableDeclaratorId(id: hydra.ext.java.syntax.Identifier): hydra.ext.java.syntax.VariableDeclaratorId = hydra.ext.java.syntax.VariableDeclaratorId(id, None)

def javaVariableDeclarator(id: hydra.ext.java.syntax.Identifier)(minit: Option[hydra.ext.java.syntax.VariableInitializer]): hydra.ext.java.syntax.VariableDeclarator =
  hydra.ext.java.syntax.VariableDeclarator(hydra.ext.java.utils.javaVariableDeclaratorId(id), minit)

def javaBoolean(b: Boolean): hydra.ext.java.syntax.Literal = hydra.ext.java.syntax.Literal.boolean(b)

def javaInt(i: BigInt): hydra.ext.java.syntax.Literal = hydra.ext.java.syntax.Literal.integer(i)

def javaString(s: scala.Predef.String): hydra.ext.java.syntax.Literal = hydra.ext.java.syntax.Literal.string(s)

def javaLiteralToJavaPrimary(lit: hydra.ext.java.syntax.Literal): hydra.ext.java.syntax.Primary =
  hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.literal(lit))

def javaExpressionToJavaPrimary(e: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Primary =
  {
  lazy val fallback: hydra.ext.java.syntax.Primary = hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.parens(e))
  e match
    case hydra.ext.java.syntax.Expression.assignment(v_Expression_assignment_ae) => v_Expression_assignment_ae match
      case hydra.ext.java.syntax.AssignmentExpression.conditional(v_AssignmentExpression_conditional_ce) => v_AssignmentExpression_conditional_ce match
        case hydra.ext.java.syntax.ConditionalExpression.simple(v_ConditionalExpression_simple_cor) => {
          lazy val cands: Seq[hydra.ext.java.syntax.ConditionalAndExpression] = v_ConditionalExpression_simple_cor
          hydra.lib.logic.ifElse[hydra.ext.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.ConditionalAndExpression](cands))(1))({
            lazy val iors: Seq[hydra.ext.java.syntax.InclusiveOrExpression] = hydra.lib.lists.head[hydra.ext.java.syntax.ConditionalAndExpression](cands)
            hydra.lib.logic.ifElse[hydra.ext.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.InclusiveOrExpression](iors))(1))({
              lazy val xors: Seq[hydra.ext.java.syntax.ExclusiveOrExpression] = hydra.lib.lists.head[hydra.ext.java.syntax.InclusiveOrExpression](iors)
              hydra.lib.logic.ifElse[hydra.ext.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.ExclusiveOrExpression](xors))(1))({
                lazy val ands: Seq[hydra.ext.java.syntax.AndExpression] = hydra.lib.lists.head[hydra.ext.java.syntax.ExclusiveOrExpression](xors)
                hydra.lib.logic.ifElse[hydra.ext.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.AndExpression](ands))(1))({
                  lazy val eqs: Seq[hydra.ext.java.syntax.EqualityExpression] = hydra.lib.lists.head[hydra.ext.java.syntax.AndExpression](ands)
                  hydra.lib.logic.ifElse[hydra.ext.java.syntax.Primary](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.EqualityExpression](eqs))(1))(hydra.lib.lists.head[hydra.ext.java.syntax.EqualityExpression](eqs) match
                    case hydra.ext.java.syntax.EqualityExpression.unary(v_EqualityExpression_unary_rel) => v_EqualityExpression_unary_rel match
                      case hydra.ext.java.syntax.RelationalExpression.simple(v_RelationalExpression_simple_shift) => v_RelationalExpression_simple_shift match
                        case hydra.ext.java.syntax.ShiftExpression.unary(v_ShiftExpression_unary_add) => v_ShiftExpression_unary_add match
                          case hydra.ext.java.syntax.AdditiveExpression.unary(v_AdditiveExpression_unary_mul) => v_AdditiveExpression_unary_mul match
                            case hydra.ext.java.syntax.MultiplicativeExpression.unary(v_MultiplicativeExpression_unary_unary) => v_MultiplicativeExpression_unary_unary match
                              case hydra.ext.java.syntax.UnaryExpression.other(v_UnaryExpression_other_npm) => v_UnaryExpression_other_npm match
                                case hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(v_UnaryExpressionNotPlusMinus_postfix_pf) => v_UnaryExpressionNotPlusMinus_postfix_pf match
                                  case hydra.ext.java.syntax.PostfixExpression.primary(v_PostfixExpression_primary_p) => v_PostfixExpression_primary_p
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

def javaPrimaryToJavaUnaryExpression(p: hydra.ext.java.syntax.Primary): hydra.ext.java.syntax.UnaryExpression =
  hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(p)))

def javaPrimaryToJavaExpression(p: hydra.ext.java.syntax.Primary): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(p))))))))))))))))

def javaPostfixExpressionToJavaUnaryExpression(pe: hydra.ext.java.syntax.PostfixExpression): hydra.ext.java.syntax.UnaryExpression =
  hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe))

def javaPostfixExpressionToJavaExpression(pe: hydra.ext.java.syntax.PostfixExpression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe)))))))))))))))

def javaPostfixExpressionToJavaRelationalExpression(pe: hydra.ext.java.syntax.PostfixExpression): hydra.ext.java.syntax.RelationalExpression =
  hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe))))))

def javaUnaryExpressionToJavaRelationalExpression(ue: hydra.ext.java.syntax.UnaryExpression): hydra.ext.java.syntax.RelationalExpression =
  hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(ue))))

def javaUnaryExpressionToJavaExpression(ue: hydra.ext.java.syntax.UnaryExpression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(ue)))))))))))))

def javaRelationalExpressionToJavaExpression(re: hydra.ext.java.syntax.RelationalExpression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.utils.javaEqualityExpressionToJavaExpression(hydra.ext.java.syntax.EqualityExpression.unary(re))

def javaRelationalExpressionToJavaUnaryExpression(re: hydra.ext.java.syntax.RelationalExpression): hydra.ext.java.syntax.UnaryExpression =
  hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.parens(hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(re))))))))))))))

def javaMultiplicativeExpressionToJavaRelationalExpression(me: hydra.ext.java.syntax.MultiplicativeExpression): hydra.ext.java.syntax.RelationalExpression =
  hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(me)))

def javaLiteralToJavaMultiplicativeExpression(lit: hydra.ext.java.syntax.Literal): hydra.ext.java.syntax.MultiplicativeExpression =
  hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.literal(lit))))))

def javaLiteralToJavaRelationalExpression(lit: hydra.ext.java.syntax.Literal): hydra.ext.java.syntax.RelationalExpression =
  hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.literal(lit)))))))))

def javaLiteralToJavaExpression(lit: hydra.ext.java.syntax.Literal): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.literal(lit))))))))))))))))))

def javaIdentifierToJavaExpressionName(id: hydra.ext.java.syntax.Identifier): hydra.ext.java.syntax.ExpressionName = hydra.ext.java.syntax.ExpressionName(None, id)

def javaIdentifierToJavaExpression(id: hydra.ext.java.syntax.Identifier): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.name(hydra.ext.java.syntax.ExpressionName(None, id)))))))))))))))))

def javaIdentifierToJavaRelationalExpression(id: hydra.ext.java.syntax.Identifier): hydra.ext.java.syntax.RelationalExpression =
  hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.name(hydra.ext.java.syntax.ExpressionName(None, id))))))))

def javaIdentifierToJavaUnaryExpression(id: hydra.ext.java.syntax.Identifier): hydra.ext.java.syntax.UnaryExpression =
  hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.name(hydra.ext.java.syntax.ExpressionName(None, id))))

def javaExpressionNameToJavaExpression(en: hydra.ext.java.syntax.ExpressionName): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.name(en))))))))))))))))

def javaFieldAccessToJavaExpression(fa: hydra.ext.java.syntax.FieldAccess): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.fieldAccess(fa))))))))))))))))))

def javaCastExpressionToJavaExpression(ce: hydra.ext.java.syntax.CastExpression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.cast(ce)))))))))))))))

def javaMethodInvocationToJavaPrimary(mi: hydra.ext.java.syntax.MethodInvocation): hydra.ext.java.syntax.Primary =
  hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.methodInvocation(mi))

def javaMethodInvocationToJavaExpression(mi: hydra.ext.java.syntax.MethodInvocation): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.methodInvocation(mi))))))))))))))))))

def javaMethodInvocationToJavaPostfixExpression(mi: hydra.ext.java.syntax.MethodInvocation): hydra.ext.java.syntax.PostfixExpression =
  hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.methodInvocation(mi)))

def javaMethodInvocationToJavaStatement(mi: hydra.ext.java.syntax.MethodInvocation): hydra.ext.java.syntax.Statement =
  hydra.ext.java.syntax.Statement.withoutTrailing(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.expression(hydra.ext.java.syntax.StatementExpression.methodInvocation(mi)))

def javaConditionalAndExpressionToJavaExpression(cae: hydra.ext.java.syntax.ConditionalAndExpression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(cae))))

def javaEqualityExpressionToJavaInclusiveOrExpression(ee: hydra.ext.java.syntax.EqualityExpression): hydra.ext.java.syntax.InclusiveOrExpression = Seq(Seq(Seq(ee)))

def javaEqualityExpressionToJavaExpression(ee: hydra.ext.java.syntax.EqualityExpression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(ee))))))))

def javaPostfixExpressionToJavaEqualityExpression(pe: hydra.ext.java.syntax.PostfixExpression): hydra.ext.java.syntax.EqualityExpression =
  hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe)))))))

def javaPostfixExpressionToJavaInclusiveOrExpression(pe: hydra.ext.java.syntax.PostfixExpression): hydra.ext.java.syntax.InclusiveOrExpression =
  Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(pe))))))))))

def javaAdditiveExpressionToJavaExpression(ae: hydra.ext.java.syntax.AdditiveExpression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(ae)))))))))))

def javaExpressionToJavaUnaryExpression(e: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.UnaryExpression =
  hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.parens(e)))))

def javaPrimitiveTypeToJavaType(pt: hydra.ext.java.syntax.PrimitiveType): hydra.ext.java.syntax.Type =
  hydra.ext.java.syntax.Type.primitive(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(pt, Seq()))

def javaClassTypeToJavaType(ct: hydra.ext.java.syntax.ClassType): hydra.ext.java.syntax.Type =
  hydra.ext.java.syntax.Type.reference(hydra.ext.java.syntax.ReferenceType.classOrInterface(hydra.ext.java.syntax.ClassOrInterfaceType.`class`(ct)))

def javaTypeVariableToType(tv: hydra.ext.java.syntax.TypeVariable): hydra.ext.java.syntax.Type =
  hydra.ext.java.syntax.Type.reference(hydra.ext.java.syntax.ReferenceType.variable(tv))

def javaRefType(args: Seq[hydra.ext.java.syntax.ReferenceType])(pkg: Option[hydra.ext.java.syntax.PackageName])(id: scala.Predef.String): hydra.ext.java.syntax.Type =
  hydra.ext.java.syntax.Type.reference(hydra.ext.java.syntax.ReferenceType.classOrInterface(hydra.ext.java.syntax.ClassOrInterfaceType.`class`(hydra.ext.java.utils.javaClassType(args)(pkg)(id))))

def javaClassType(args: Seq[hydra.ext.java.syntax.ReferenceType])(pkg: Option[hydra.ext.java.syntax.PackageName])(id: scala.Predef.String): hydra.ext.java.syntax.ClassType =
  {
  lazy val qual: hydra.ext.java.syntax.ClassTypeQualifier = hydra.lib.maybes.cases[hydra.ext.java.syntax.PackageName, hydra.ext.java.syntax.ClassTypeQualifier](pkg)(hydra.ext.java.syntax.ClassTypeQualifier.none)((p: hydra.ext.java.syntax.PackageName) => hydra.ext.java.syntax.ClassTypeQualifier.`package`(p))
  lazy val targs: Seq[hydra.ext.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.TypeArgument]((rt: hydra.ext.java.syntax.ReferenceType) => hydra.ext.java.syntax.TypeArgument.reference(rt))(args)
  hydra.ext.java.syntax.ClassType(Seq(), qual, hydra.ext.java.utils.javaTypeIdentifier(id), targs)
}

def javaTypeVariable(v: scala.Predef.String): hydra.ext.java.syntax.ReferenceType =
  hydra.ext.java.syntax.ReferenceType.variable(hydra.ext.java.syntax.TypeVariable(Seq(), hydra.ext.java.utils.javaTypeIdentifier(hydra.formatting.capitalize(v))))

lazy val javaBooleanType: hydra.ext.java.syntax.Type = hydra.ext.java.utils.javaPrimitiveTypeToJavaType(hydra.ext.java.syntax.PrimitiveType.boolean)

lazy val javaIntType: hydra.ext.java.syntax.Type = hydra.ext.java.utils.javaPrimitiveTypeToJavaType(hydra.ext.java.syntax.PrimitiveType.numeric(hydra.ext.java.syntax.NumericType.integral(hydra.ext.java.syntax.IntegralType.int)))

def javaBooleanExpression(b: Boolean): hydra.ext.java.syntax.Expression =
  hydra.ext.java.utils.javaPrimaryToJavaExpression(hydra.ext.java.utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.javaBoolean(b)))

def javaIntExpression(i: BigInt): hydra.ext.java.syntax.Expression =
  hydra.ext.java.utils.javaPrimaryToJavaExpression(hydra.ext.java.utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.javaInt(i)))

def javaCastExpression(rt: hydra.ext.java.syntax.ReferenceType)(expr: hydra.ext.java.syntax.UnaryExpression): hydra.ext.java.syntax.CastExpression =
  hydra.ext.java.syntax.CastExpression.notPlusMinus(hydra.ext.java.syntax.CastExpression_NotPlusMinus(hydra.ext.java.syntax.CastExpression_RefAndBounds(rt, Seq()), expr))

def javaCastPrimitive(pt: hydra.ext.java.syntax.PrimitiveType)(expr: hydra.ext.java.syntax.UnaryExpression): hydra.ext.java.syntax.CastExpression =
  hydra.ext.java.syntax.CastExpression.primitive(hydra.ext.java.syntax.CastExpression_Primitive(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(pt, Seq()), expr))

def javaReturnStatement(mex: Option[hydra.ext.java.syntax.Expression]): hydra.ext.java.syntax.Statement =
  hydra.ext.java.syntax.Statement.withoutTrailing(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.`return`(mex))

def javaThrowStatement(e: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Statement =
  hydra.ext.java.syntax.Statement.withoutTrailing(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.`throw`(e))

lazy val javaEmptyStatement: hydra.ext.java.syntax.Statement = hydra.ext.java.syntax.Statement.withoutTrailing(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.empty)

def javaAssignmentStatement(lhs: hydra.ext.java.syntax.LeftHandSide)(rhs: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Statement =
  hydra.ext.java.syntax.Statement.withoutTrailing(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.expression(hydra.ext.java.syntax.StatementExpression.assignment(hydra.ext.java.syntax.Assignment(lhs, hydra.ext.java.syntax.AssignmentOperator.simple, rhs))))

def javaStatementsToBlock(stmts: Seq[hydra.ext.java.syntax.Statement]): hydra.ext.java.syntax.Block =
  hydra.lib.lists.map[hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.BlockStatement]((s: hydra.ext.java.syntax.Statement) => hydra.ext.java.syntax.BlockStatement.statement(s))(stmts)

def javaLambda(v: hydra.core.Name)(body: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.lambda(hydra.ext.java.syntax.LambdaExpression(hydra.ext.java.syntax.LambdaParameters.single(hydra.ext.java.utils.variableToJavaIdentifier(v)), hydra.ext.java.syntax.LambdaBody.expression(body)))

def javaLambdaFromBlock(v: hydra.core.Name)(block: hydra.ext.java.syntax.Block): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.lambda(hydra.ext.java.syntax.LambdaExpression(hydra.ext.java.syntax.LambdaParameters.single(hydra.ext.java.utils.variableToJavaIdentifier(v)), hydra.ext.java.syntax.LambdaBody.block(block)))

def javaMethodBody(mstmts: Option[Seq[hydra.ext.java.syntax.BlockStatement]]): hydra.ext.java.syntax.MethodBody =
  hydra.lib.maybes.cases[Seq[hydra.ext.java.syntax.BlockStatement], hydra.ext.java.syntax.MethodBody](mstmts)(hydra.ext.java.syntax.MethodBody.none)((stmts: Seq[hydra.ext.java.syntax.BlockStatement]) => hydra.ext.java.syntax.MethodBody.block(stmts))

def javaMethodHeader(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(methodName: scala.Predef.String)(params: Seq[hydra.ext.java.syntax.FormalParameter])(result: hydra.ext.java.syntax.Result): hydra.ext.java.syntax.MethodHeader =
  hydra.ext.java.syntax.MethodHeader(tparams, result, hydra.ext.java.syntax.MethodDeclarator(methodName, None, params), None)

def javaMethodDeclarationToJavaClassBodyDeclaration(md: hydra.ext.java.syntax.MethodDeclaration): hydra.ext.java.syntax.ClassBodyDeclaration =
  hydra.ext.java.syntax.ClassBodyDeclaration.classMember(hydra.ext.java.syntax.ClassMemberDeclaration.method(md))

def javaInterfaceDeclarationToJavaClassBodyDeclaration(nid: hydra.ext.java.syntax.NormalInterfaceDeclaration): hydra.ext.java.syntax.ClassBodyDeclaration =
  hydra.ext.java.syntax.ClassBodyDeclaration.classMember(hydra.ext.java.syntax.ClassMemberDeclaration.interface(hydra.ext.java.syntax.InterfaceDeclaration.normalInterface(nid)))

def javaMemberField(mods: Seq[hydra.ext.java.syntax.FieldModifier])(jt: hydra.ext.java.syntax.Type)(v: hydra.ext.java.syntax.VariableDeclarator): hydra.ext.java.syntax.ClassBodyDeclaration =
  hydra.ext.java.syntax.ClassBodyDeclaration.classMember(hydra.ext.java.syntax.ClassMemberDeclaration.field(hydra.ext.java.syntax.FieldDeclaration(mods, jt, Seq(v))))

def javaTypeToJavaFormalParameter(jt: hydra.ext.java.syntax.Type)(fname: hydra.core.Name): hydra.ext.java.syntax.FormalParameter =
  hydra.ext.java.syntax.FormalParameter.simple(hydra.ext.java.syntax.FormalParameter_Simple(Seq(), jt, hydra.ext.java.utils.fieldNameToJavaVariableDeclaratorId(fname)))

def javaTypeToJavaResult(jt: hydra.ext.java.syntax.Type): hydra.ext.java.syntax.Result = hydra.ext.java.syntax.Result.`type`(jt)

def javaTypeToJavaTypeArgument(t: hydra.ext.java.syntax.Type): hydra.ext.java.syntax.TypeArgument =
  t match
  case hydra.ext.java.syntax.Type.reference(v_Type_reference_rt) => hydra.ext.java.syntax.TypeArgument.reference(v_Type_reference_rt)
  case hydra.ext.java.syntax.Type.primitive(_) => hydra.ext.java.syntax.TypeArgument.wildcard(hydra.ext.java.syntax.Wildcard(Seq(), None))

def referenceTypeToResult(rt: hydra.ext.java.syntax.ReferenceType): hydra.ext.java.syntax.Result =
  hydra.ext.java.utils.javaTypeToJavaResult(hydra.ext.java.syntax.Type.reference(rt))

def javaConstructorName(id: hydra.ext.java.syntax.Identifier)(targs: Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]): hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate =
  hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate(Seq(hydra.ext.java.syntax.AnnotatedIdentifier(Seq(), id)), targs)

def javaConstructorCall(ci: hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate)(args: Seq[hydra.ext.java.syntax.Expression])(mbody: Option[hydra.ext.java.syntax.ClassBody]): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.classInstance(hydra.ext.java.syntax.ClassInstanceCreationExpression(None, hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression(Seq(), ci, args, mbody))))))))))))))))))))

lazy val javaThis: hydra.ext.java.syntax.Expression = hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.`this`)))))))))))))))))

def javaTypeParameter(v: scala.Predef.String): hydra.ext.java.syntax.TypeParameter =
  hydra.ext.java.syntax.TypeParameter(Seq(), hydra.ext.java.utils.javaTypeIdentifier(v), None)

def javaTypeIdentifierToJavaTypeArgument(id: hydra.ext.java.syntax.TypeIdentifier): hydra.ext.java.syntax.TypeArgument =
  hydra.ext.java.syntax.TypeArgument.reference(hydra.ext.java.syntax.ReferenceType.variable(hydra.ext.java.syntax.TypeVariable(Seq(), id)))

def typeParameterToTypeArgument(tp: hydra.ext.java.syntax.TypeParameter): hydra.ext.java.syntax.TypeArgument = hydra.ext.java.utils.javaTypeIdentifierToJavaTypeArgument(tp.identifier)

def typeParameterToReferenceType(tp: hydra.ext.java.syntax.TypeParameter): hydra.ext.java.syntax.ReferenceType = hydra.ext.java.utils.javaTypeVariable(tp.identifier)

def fieldNameToJavaIdentifier(fname: hydra.core.Name): hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.javaIdentifier(fname)

def fieldNameToJavaExpression(fname: hydra.core.Name): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.assignment(hydra.ext.java.syntax.AssignmentExpression.conditional(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(Seq(Seq(Seq(hydra.ext.java.syntax.EqualityExpression.unary(hydra.ext.java.syntax.RelationalExpression.simple(hydra.ext.java.syntax.ShiftExpression.unary(hydra.ext.java.syntax.AdditiveExpression.unary(hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.postfix(hydra.ext.java.syntax.PostfixExpression.name(hydra.ext.java.utils.javaIdentifierToJavaExpressionName(hydra.ext.java.utils.fieldNameToJavaIdentifier(fname))))))))))))))))))

def fieldNameToJavaVariableDeclaratorId(fname: hydra.core.Name): hydra.ext.java.syntax.VariableDeclaratorId =
  hydra.ext.java.utils.javaVariableDeclaratorId(hydra.ext.java.utils.javaIdentifier(fname))

def fieldNameToJavaVariableDeclarator(fname: hydra.core.Name): hydra.ext.java.syntax.VariableDeclarator =
  hydra.ext.java.utils.javaVariableDeclarator(hydra.ext.java.utils.javaIdentifier(fname))(None)

def fieldExpression(varId: hydra.ext.java.syntax.Identifier)(fieldId: hydra.ext.java.syntax.Identifier): hydra.ext.java.syntax.ExpressionName = hydra.ext.java.syntax.ExpressionName(Some(Seq(varId)), fieldId)

def variableToJavaIdentifier(name: hydra.core.Name): hydra.ext.java.syntax.Identifier =
  {
  lazy val v: scala.Predef.String = name
  hydra.lib.logic.ifElse[hydra.ext.java.syntax.Identifier](hydra.lib.equality.equal[scala.Predef.String](v)("_"))("ignored")(hydra.ext.java.utils.sanitizeJavaName(v))
}

def varDeclarationStatement(id: hydra.ext.java.syntax.Identifier)(rhs: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.BlockStatement =
  hydra.ext.java.syntax.BlockStatement.localVariableDeclaration(hydra.ext.java.syntax.LocalVariableDeclaration(Seq(), hydra.ext.java.syntax.LocalVariableType.`var`, Seq(hydra.ext.java.utils.javaVariableDeclarator(id)(Some(hydra.ext.java.syntax.VariableInitializer.expression(rhs))))))

def sanitizeJavaName(name: scala.Predef.String): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.ext.java.utils.isEscaped(name))(hydra.ext.java.utils.unescape(name))(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](name)("_"))("ignored")(hydra.formatting.sanitizeWithUnderscores(hydra.ext.java.language.reservedWords)(name)))

def isEscaped(s: scala.Predef.String): Boolean = hydra.lib.equality.equal[Int](hydra.lib.strings.charAt(0)(s))(36)

def unescape(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.strings.fromList(hydra.lib.lists.tail[Int](hydra.lib.strings.toList(s)))

def javaPackageDeclaration(ns: hydra.module.Namespace): hydra.ext.java.syntax.PackageDeclaration =
  hydra.ext.java.syntax.PackageDeclaration(Seq(), hydra.lib.lists.map[scala.Predef.String, hydra.ext.java.syntax.Identifier]((s: scala.Predef.String) => s)(hydra.lib.strings.splitOn(".")(ns)))

lazy val overrideAnnotation: hydra.ext.java.syntax.Annotation = hydra.ext.java.syntax.Annotation.marker(hydra.ext.java.utils.javaTypeName("Override"))

def methodInvocation(lhs: Option[Either[hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary]])(methodName: hydra.ext.java.syntax.Identifier)(args: Seq[hydra.ext.java.syntax.Expression]): hydra.ext.java.syntax.MethodInvocation =
  {
  lazy val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.lib.maybes.cases[Either[hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary], hydra.ext.java.syntax.MethodInvocation_Header](lhs)(hydra.ext.java.syntax.MethodInvocation_Header.simple(methodName))((either: Either[hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary]) =>
    hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.lib.eithers.either[hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.MethodInvocation_Variant]((en: hydra.ext.java.syntax.ExpressionName) =>
    hydra.ext.java.syntax.MethodInvocation_Variant.expression(en))((p: hydra.ext.java.syntax.Primary) => hydra.ext.java.syntax.MethodInvocation_Variant.primary(p))(either), Seq(), methodName)))
  hydra.ext.java.syntax.MethodInvocation(header, args)
}

def methodInvocationStatic(self: hydra.ext.java.syntax.Identifier)(methodName: hydra.ext.java.syntax.Identifier)(args: Seq[hydra.ext.java.syntax.Expression]): hydra.ext.java.syntax.MethodInvocation =
  hydra.ext.java.utils.methodInvocation(Some(Left(hydra.ext.java.utils.javaIdentifierToJavaExpressionName(self))))(methodName)(args)

def methodDeclaration(mods: Seq[hydra.ext.java.syntax.MethodModifier])(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(anns: Seq[hydra.ext.java.syntax.Annotation])(methodName: scala.Predef.String)(params: Seq[hydra.ext.java.syntax.FormalParameter])(result: hydra.ext.java.syntax.Result)(stmts: Option[Seq[hydra.ext.java.syntax.BlockStatement]]): hydra.ext.java.syntax.ClassBodyDeclaration =
  hydra.ext.java.utils.javaMethodDeclarationToJavaClassBodyDeclaration(hydra.ext.java.syntax.MethodDeclaration(anns, mods, hydra.ext.java.utils.javaMethodHeader(tparams)(methodName)(params)(result), hydra.ext.java.utils.javaMethodBody(stmts)))

def interfaceMethodDeclaration(mods: Seq[hydra.ext.java.syntax.InterfaceMethodModifier])(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(methodName: scala.Predef.String)(params: Seq[hydra.ext.java.syntax.FormalParameter])(result: hydra.ext.java.syntax.Result)(stmts: Option[Seq[hydra.ext.java.syntax.BlockStatement]]): hydra.ext.java.syntax.InterfaceMemberDeclaration =
  hydra.ext.java.syntax.InterfaceMemberDeclaration.interfaceMethod(hydra.ext.java.syntax.InterfaceMethodDeclaration(mods, hydra.ext.java.utils.javaMethodHeader(tparams)(methodName)(params)(result), hydra.ext.java.utils.javaMethodBody(stmts)))

def javaEquals(lhs: hydra.ext.java.syntax.EqualityExpression)(rhs: hydra.ext.java.syntax.RelationalExpression): hydra.ext.java.syntax.EqualityExpression =
  hydra.ext.java.syntax.EqualityExpression.equal(hydra.ext.java.syntax.EqualityExpression_Binary(lhs, rhs))

def javaEqualsNull(lhs: hydra.ext.java.syntax.EqualityExpression): hydra.ext.java.syntax.EqualityExpression =
  hydra.ext.java.utils.javaEquals(lhs)(hydra.ext.java.utils.javaLiteralToJavaRelationalExpression(hydra.ext.java.syntax.Literal.`null`))

def javaInstanceOf(lhs: hydra.ext.java.syntax.RelationalExpression)(rhs: hydra.ext.java.syntax.ReferenceType): hydra.ext.java.syntax.RelationalExpression =
  hydra.ext.java.syntax.RelationalExpression.instanceof(hydra.ext.java.syntax.RelationalExpression_InstanceOf(lhs, rhs))

def javaThrowIllegalArgumentException(args: Seq[hydra.ext.java.syntax.Expression]): hydra.ext.java.syntax.Statement =
  hydra.ext.java.utils.javaThrowStatement(hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName("IllegalArgumentException")(None))(args)(None))

def javaThrowIllegalStateException(args: Seq[hydra.ext.java.syntax.Expression]): hydra.ext.java.syntax.Statement =
  hydra.ext.java.utils.javaThrowStatement(hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName("IllegalStateException")(None))(args)(None))

def addExpressions(exprs: Seq[hydra.ext.java.syntax.MultiplicativeExpression]): hydra.ext.java.syntax.AdditiveExpression =
  {
  lazy val first: hydra.ext.java.syntax.AdditiveExpression = hydra.ext.java.syntax.AdditiveExpression.unary(hydra.lib.lists.head[hydra.ext.java.syntax.MultiplicativeExpression](exprs))
  lazy val rest: Seq[hydra.ext.java.syntax.MultiplicativeExpression] = hydra.lib.lists.tail[hydra.ext.java.syntax.MultiplicativeExpression](exprs)
  hydra.lib.lists.foldl[hydra.ext.java.syntax.AdditiveExpression, hydra.ext.java.syntax.MultiplicativeExpression]((ae: hydra.ext.java.syntax.AdditiveExpression) =>
    (me: hydra.ext.java.syntax.MultiplicativeExpression) =>
    hydra.ext.java.syntax.AdditiveExpression.plus(hydra.ext.java.syntax.AdditiveExpression_Binary(ae, me)))(first)(rest)
}

def javaRelationalExpressionToJavaEqualityExpression(re: hydra.ext.java.syntax.RelationalExpression): hydra.ext.java.syntax.EqualityExpression = hydra.ext.java.syntax.EqualityExpression.unary(re)

def nameToQualifiedJavaName(aliases: hydra.ext.java.environment.Aliases)(qualify: Boolean)(name: hydra.core.Name)(mlocal: Option[scala.Predef.String]): Tuple2[hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier] =
  {
  lazy val qn: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  lazy val `ns_`: Option[hydra.module.Namespace] = (qn.namespace)
  lazy val local: scala.Predef.String = (qn.local)
  lazy val alias: Option[hydra.ext.java.syntax.PackageName] = hydra.lib.maybes.cases[hydra.module.Namespace, Option[hydra.ext.java.syntax.PackageName]](`ns_`)(None)((n: hydra.module.Namespace) =>
    Some(hydra.lib.maybes.cases[hydra.ext.java.syntax.PackageName, hydra.ext.java.syntax.PackageName](hydra.lib.maps.lookup[hydra.module.Namespace, hydra.ext.java.syntax.PackageName](n)(aliases.packages))(hydra.ext.java.names.javaPackageName(hydra.lib.strings.splitOn(".")(n)))((id: hydra.ext.java.syntax.PackageName) => id)))
  lazy val pkg: hydra.ext.java.syntax.ClassTypeQualifier = hydra.lib.logic.ifElse[hydra.ext.java.syntax.ClassTypeQualifier](qualify)(hydra.lib.maybes.cases[hydra.ext.java.syntax.PackageName, hydra.ext.java.syntax.ClassTypeQualifier](alias)(hydra.ext.java.syntax.ClassTypeQualifier.none)((p: hydra.ext.java.syntax.PackageName) => hydra.ext.java.syntax.ClassTypeQualifier.`package`(p)))(hydra.ext.java.syntax.ClassTypeQualifier.none)
  lazy val jid: hydra.ext.java.syntax.TypeIdentifier = hydra.ext.java.utils.javaTypeIdentifier(hydra.lib.maybes.cases[scala.Predef.String, scala.Predef.String](mlocal)(hydra.ext.java.utils.sanitizeJavaName(local))((l: scala.Predef.String) =>
    hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.ext.java.utils.sanitizeJavaName(local))("."))(hydra.ext.java.utils.sanitizeJavaName(l))))
  Tuple2(jid, pkg)
}

def nameToJavaClassType(aliases: hydra.ext.java.environment.Aliases)(qualify: Boolean)(args: Seq[hydra.ext.java.syntax.TypeArgument])(name: hydra.core.Name)(mlocal: Option[scala.Predef.String]): hydra.ext.java.syntax.ClassType =
  {
  lazy val result: Tuple2[hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier] = hydra.ext.java.utils.nameToQualifiedJavaName(aliases)(qualify)(name)(mlocal)
  lazy val id: hydra.ext.java.syntax.TypeIdentifier = hydra.lib.pairs.first[hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier](result)
  lazy val pkg: hydra.ext.java.syntax.ClassTypeQualifier = hydra.lib.pairs.second[hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier](result)
  hydra.ext.java.syntax.ClassType(Seq(), pkg, id, args)
}

def nameToJavaReferenceType(aliases: hydra.ext.java.environment.Aliases)(qualify: Boolean)(args: Seq[hydra.ext.java.syntax.TypeArgument])(name: hydra.core.Name)(mlocal: Option[scala.Predef.String]): hydra.ext.java.syntax.ReferenceType =
  hydra.ext.java.syntax.ReferenceType.classOrInterface(hydra.ext.java.syntax.ClassOrInterfaceType.`class`(hydra.ext.java.utils.nameToJavaClassType(aliases)(qualify)(args)(name)(mlocal)))

def nameToJavaName(aliases: hydra.ext.java.environment.Aliases)(name: hydra.core.Name): hydra.ext.java.syntax.Identifier =
  {
  lazy val qn: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  lazy val `ns_`: Option[hydra.module.Namespace] = (qn.namespace)
  lazy val local: scala.Predef.String = (qn.local)
  hydra.lib.logic.ifElse[hydra.ext.java.syntax.Identifier](hydra.ext.java.utils.isEscaped(name))(hydra.ext.java.utils.sanitizeJavaName(local))(hydra.lib.maybes.cases[hydra.module.Namespace, hydra.ext.java.syntax.Identifier](`ns_`)(local)((gname: hydra.module.Namespace) =>
    {
    lazy val parts: Seq[scala.Predef.String] = hydra.lib.maybes.cases[hydra.ext.java.syntax.PackageName, Seq[scala.Predef.String]](hydra.lib.maps.lookup[hydra.module.Namespace, hydra.ext.java.syntax.PackageName](gname)(aliases.packages))(hydra.lib.strings.splitOn(".")(gname))((pkgName: hydra.ext.java.syntax.PackageName) =>
      hydra.lib.lists.map[hydra.ext.java.syntax.Identifier, scala.Predef.String]((i: hydra.ext.java.syntax.Identifier) => i)(pkgName))
    lazy val allParts: Seq[scala.Predef.String] = hydra.lib.lists.concat2[scala.Predef.String](parts)(Seq(hydra.ext.java.utils.sanitizeJavaName(local)))
    hydra.lib.strings.intercalate(".")(allParts)
  }))
}

def nameToJavaTypeIdentifier(aliases: hydra.ext.java.environment.Aliases)(qualify: Boolean)(name: hydra.core.Name): hydra.ext.java.syntax.TypeIdentifier =
  hydra.lib.pairs.first[hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier](hydra.ext.java.utils.nameToQualifiedJavaName(aliases)(qualify)(name)(None))

def javaTypeFromTypeName(aliases: hydra.ext.java.environment.Aliases)(elName: hydra.core.Name): hydra.ext.java.syntax.Type =
  hydra.ext.java.utils.javaTypeVariableToType(hydra.ext.java.syntax.TypeVariable(Seq(), hydra.ext.java.utils.nameToJavaTypeIdentifier(aliases)(false)(elName)))

def javaDoubleCastExpression(rawRt: hydra.ext.java.syntax.ReferenceType)(targetRt: hydra.ext.java.syntax.ReferenceType)(expr: hydra.ext.java.syntax.UnaryExpression): hydra.ext.java.syntax.CastExpression =
  {
  lazy val firstCast: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(rawRt)(expr))
  hydra.ext.java.utils.javaCastExpression(targetRt)(hydra.ext.java.utils.javaExpressionToJavaUnaryExpression(firstCast))
}

def javaDoubleCastExpressionToJavaExpression(rawRt: hydra.ext.java.syntax.ReferenceType)(targetRt: hydra.ext.java.syntax.ReferenceType)(expr: hydra.ext.java.syntax.UnaryExpression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaDoubleCastExpression(rawRt)(targetRt)(expr))

lazy val javaBytePrimitiveType: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations = hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(hydra.ext.java.syntax.PrimitiveType.numeric(hydra.ext.java.syntax.NumericType.integral(hydra.ext.java.syntax.IntegralType.byte)), Seq())

lazy val visitorTypeVariable: hydra.ext.java.syntax.ReferenceType = hydra.ext.java.utils.javaTypeVariable("r")

def lookupJavaVarName(aliases: hydra.ext.java.environment.Aliases)(name: hydra.core.Name): hydra.core.Name =
  hydra.lib.maybes.cases[hydra.core.Name, hydra.core.Name](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Name](name)(aliases.varRenames))(name)((renamed: hydra.core.Name) => renamed)

def variantClassName(qualify: Boolean)(elName: hydra.core.Name)(fname: hydra.core.Name): hydra.core.Name =
  {
  lazy val qn: hydra.module.QualifiedName = hydra.names.qualifyName(elName)
  lazy val `ns_`: Option[hydra.module.Namespace] = (qn.namespace)
  lazy val local: scala.Predef.String = (qn.local)
  lazy val flocal: scala.Predef.String = hydra.formatting.capitalize(fname)
  lazy val local1: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](qualify)(hydra.lib.strings.cat2(hydra.lib.strings.cat2(local)("."))(flocal))(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](flocal)(local))(hydra.lib.strings.cat2(flocal)("_"))(flocal))
  hydra.names.unqualifyName(hydra.module.QualifiedName(`ns_`, local1))
}

def variableDeclarationStatement[T0](aliases: T0)(jtype: hydra.ext.java.syntax.Type)(id: hydra.ext.java.syntax.Identifier)(rhs: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.BlockStatement =
  {
  lazy val `init_`: hydra.ext.java.syntax.VariableInitializer = hydra.ext.java.syntax.VariableInitializer.expression(rhs)
  lazy val vdec: hydra.ext.java.syntax.VariableDeclarator = hydra.ext.java.utils.javaVariableDeclarator(id)(Some(`init_`))
  hydra.ext.java.syntax.BlockStatement.localVariableDeclaration(hydra.ext.java.syntax.LocalVariableDeclaration(Seq(), hydra.ext.java.syntax.LocalVariableType.`type`(jtype), Seq(vdec)))
}

def finalVarDeclarationStatement(id: hydra.ext.java.syntax.Identifier)(rhs: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.BlockStatement =
  hydra.ext.java.syntax.BlockStatement.localVariableDeclaration(hydra.ext.java.syntax.LocalVariableDeclaration(Seq(hydra.ext.java.syntax.VariableModifier.`final`), hydra.ext.java.syntax.LocalVariableType.`var`, Seq(hydra.ext.java.utils.javaVariableDeclarator(id)(Some(hydra.ext.java.syntax.VariableInitializer.expression(rhs))))))

def javaStringMultiplicativeExpression(s: scala.Predef.String): hydra.ext.java.syntax.MultiplicativeExpression =
  hydra.ext.java.utils.javaLiteralToJavaMultiplicativeExpression(hydra.ext.java.utils.javaString(s))

lazy val suppressWarningsUncheckedAnnotation: hydra.ext.java.syntax.Annotation = hydra.ext.java.syntax.Annotation.singleElement(hydra.ext.java.syntax.SingleElementAnnotation(hydra.ext.java.utils.javaTypeName("SuppressWarnings"), Some(hydra.ext.java.syntax.ElementValue.conditionalExpression(hydra.ext.java.syntax.ConditionalExpression.simple(Seq(Seq(hydra.ext.java.utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.javaString("unchecked")))))))))))

def methodInvocationStaticWithTypeArgs(self: hydra.ext.java.syntax.Identifier)(methodName: hydra.ext.java.syntax.Identifier)(targs: Seq[hydra.ext.java.syntax.TypeArgument])(args: Seq[hydra.ext.java.syntax.Expression]): hydra.ext.java.syntax.MethodInvocation =
  {
  lazy val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.expression(hydra.ext.java.utils.javaIdentifierToJavaExpressionName(self)), targs, methodName))
  hydra.ext.java.syntax.MethodInvocation(header, args)
}

def javaArrayCreation(primType: hydra.ext.java.syntax.PrimitiveTypeWithAnnotations)(minit: Option[hydra.ext.java.syntax.ArrayInitializer]): hydra.ext.java.syntax.Expression =
  {
  lazy val `init_`: hydra.ext.java.syntax.ArrayInitializer = hydra.lib.maybes.cases[hydra.ext.java.syntax.ArrayInitializer, hydra.ext.java.syntax.ArrayInitializer](minit)(Seq())((i: hydra.ext.java.syntax.ArrayInitializer) => i)
  hydra.ext.java.utils.javaPrimaryToJavaExpression(hydra.ext.java.syntax.Primary.arrayCreation(hydra.ext.java.syntax.ArrayCreationExpression.primitiveArray(hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray(primType, Seq(), `init_`))))
}

def javaArrayInitializer(exprs: Seq[hydra.ext.java.syntax.Expression]): hydra.ext.java.syntax.ArrayInitializer =
  Seq(hydra.lib.lists.map[hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.VariableInitializer]((e: hydra.ext.java.syntax.Expression) => hydra.ext.java.syntax.VariableInitializer.expression(e))(exprs))

def toAssignStmt(fname: hydra.core.Name): hydra.ext.java.syntax.Statement =
  {
  lazy val id: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.fieldNameToJavaIdentifier(fname)
  lazy val lhs: hydra.ext.java.syntax.LeftHandSide = hydra.ext.java.syntax.LeftHandSide.fieldAccess(hydra.ext.java.syntax.FieldAccess(hydra.ext.java.syntax.FieldAccess_Qualifier.primary(hydra.ext.java.syntax.Primary.noNewArray(hydra.ext.java.syntax.PrimaryNoNewArray.`this`)), id))
  lazy val rhs: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.fieldNameToJavaExpression(fname)
  hydra.ext.java.utils.javaAssignmentStatement(lhs)(rhs)
}

def unTypeParameter(tp: hydra.ext.java.syntax.TypeParameter): scala.Predef.String = (tp.identifier)

def importAliasesForModule(mod: hydra.module.Module): hydra.ext.java.environment.Aliases =
  hydra.ext.java.environment.Aliases(mod.namespace, hydra.lib.maps.empty[hydra.module.Namespace, hydra.ext.java.syntax.PackageName], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name], hydra.lib.sets.empty[hydra.core.Name], None, hydra.lib.sets.empty[hydra.core.Name])

def javaClassDeclaration(aliases: hydra.ext.java.environment.Aliases)(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(elName: hydra.core.Name)(mods: Seq[hydra.ext.java.syntax.ClassModifier])(supname: Option[hydra.core.Name])(impls: Seq[hydra.ext.java.syntax.InterfaceType])(bodyDecls: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]): hydra.ext.java.syntax.ClassDeclaration =
  {
  lazy val `extends_`: Option[hydra.ext.java.syntax.ClassType] = hydra.lib.maybes.map[hydra.core.Name, hydra.ext.java.syntax.ClassType]((n: hydra.core.Name) =>
    hydra.ext.java.utils.nameToJavaClassType(aliases)(true)(Seq())(n)(None))(supname)
  hydra.ext.java.syntax.ClassDeclaration.normal(hydra.ext.java.syntax.NormalClassDeclaration(mods, hydra.ext.java.utils.javaDeclName(elName), tparams, `extends_`, impls, bodyDecls))
}

def makeConstructor(aliases: hydra.ext.java.environment.Aliases)(elName: hydra.core.Name)(`private`: Boolean)(params: Seq[hydra.ext.java.syntax.FormalParameter])(stmts: Seq[hydra.ext.java.syntax.BlockStatement]): hydra.ext.java.syntax.ClassBodyDeclaration =
  {
  lazy val nm: hydra.ext.java.syntax.SimpleTypeName = hydra.ext.java.utils.nameToJavaTypeIdentifier(aliases)(false)(elName)
  lazy val cons: hydra.ext.java.syntax.ConstructorDeclarator = hydra.ext.java.syntax.ConstructorDeclarator(Seq(), nm, None, params)
  lazy val mods: Seq[hydra.ext.java.syntax.ConstructorModifier] = Seq(hydra.lib.logic.ifElse[hydra.ext.java.syntax.ConstructorModifier](`private`)(hydra.ext.java.syntax.ConstructorModifier.`private`)(hydra.ext.java.syntax.ConstructorModifier.public))
  lazy val body: hydra.ext.java.syntax.ConstructorBody = hydra.ext.java.syntax.ConstructorBody(None, stmts)
  hydra.ext.java.syntax.ClassBodyDeclaration.constructorDeclaration(hydra.ext.java.syntax.ConstructorDeclaration(mods, cons, None, body))
}

def toAcceptMethod(`abstract`: Boolean)(vtparams: Seq[hydra.ext.java.syntax.TypeParameter]): hydra.ext.java.syntax.ClassBodyDeclaration =
  {
  lazy val mods: Seq[hydra.ext.java.syntax.MethodModifier] = hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.MethodModifier]](`abstract`)(Seq(hydra.ext.java.syntax.MethodModifier.public, hydra.ext.java.syntax.MethodModifier.`abstract`))(Seq(hydra.ext.java.syntax.MethodModifier.public))
  lazy val tparams: Seq[hydra.ext.java.syntax.TypeParameter] = Seq(hydra.ext.java.utils.javaTypeParameter(hydra.ext.java.names.visitorReturnParameter))
  lazy val anns: Seq[hydra.ext.java.syntax.Annotation] = hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.Annotation]](`abstract`)(Seq())(Seq(hydra.ext.java.utils.overrideAnnotation))
  lazy val typeArgs: Seq[hydra.ext.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument]((tp: hydra.ext.java.syntax.TypeParameter) =>
    hydra.ext.java.syntax.TypeArgument.reference(hydra.ext.java.utils.typeParameterToReferenceType(tp)))(vtparams)
  lazy val ref: hydra.ext.java.syntax.Type = hydra.ext.java.utils.javaClassTypeToJavaType(hydra.ext.java.syntax.ClassType(Seq(), hydra.ext.java.syntax.ClassTypeQualifier.none, hydra.ext.java.utils.javaTypeIdentifier(hydra.ext.java.names.visitorName), hydra.lib.lists.concat2[hydra.ext.java.syntax.TypeArgument](typeArgs)(Seq(hydra.ext.java.syntax.TypeArgument.reference(hydra.ext.java.utils.visitorTypeVariable)))))
  lazy val param: hydra.ext.java.syntax.FormalParameter = hydra.ext.java.utils.javaTypeToJavaFormalParameter(ref)("visitor")
  lazy val result: hydra.ext.java.syntax.Result = hydra.ext.java.utils.javaTypeToJavaResult(hydra.ext.java.syntax.Type.reference(hydra.ext.java.utils.visitorTypeVariable))
  lazy val returnExpr: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStatic("visitor")(hydra.ext.java.names.visitMethodName)(Seq(hydra.ext.java.utils.javaThis)))
  lazy val body: Option[Seq[hydra.ext.java.syntax.BlockStatement]] = hydra.lib.logic.ifElse[Option[Seq[hydra.ext.java.syntax.BlockStatement]]](`abstract`)(None)(Some(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(returnExpr))))))
  hydra.ext.java.utils.methodDeclaration(mods)(tparams)(anns)(hydra.ext.java.names.acceptMethodName)(Seq(param))(result)(body)
}

def toJavaArrayType(t: hydra.ext.java.syntax.Type)(cx: hydra.context.Context): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.java.syntax.Type] =
  t match
  case hydra.ext.java.syntax.Type.reference(v_Type_reference_rt) => v_Type_reference_rt match
    case hydra.ext.java.syntax.ReferenceType.classOrInterface(v_ReferenceType_classOrInterface_cit) => Right(hydra.ext.java.syntax.Type.reference(hydra.ext.java.syntax.ReferenceType.array(hydra.ext.java.syntax.ArrayType(Seq(Seq()), hydra.ext.java.syntax.ArrayType_Variant.classOrInterface(v_ReferenceType_classOrInterface_cit)))))
    case hydra.ext.java.syntax.ReferenceType.array(v_ReferenceType_array_at) => {
      lazy val oldDims: Seq[Seq[hydra.ext.java.syntax.Annotation]] = (v_ReferenceType_array_at.dims)
      lazy val newDims: hydra.ext.java.syntax.Dims = hydra.lib.lists.concat2[Seq[hydra.ext.java.syntax.Annotation]](oldDims)(Seq(Seq()))
      lazy val variant: hydra.ext.java.syntax.ArrayType_Variant = (v_ReferenceType_array_at.variant)
      Right(hydra.ext.java.syntax.Type.reference(hydra.ext.java.syntax.ReferenceType.array(hydra.ext.java.syntax.ArrayType(newDims, variant))))
    }
    case hydra.ext.java.syntax.ReferenceType.variable(_) => Left(hydra.context.InContext(hydra.errors.Error.other("don't know how to make Java reference type into array type"), cx))
  case hydra.ext.java.syntax.Type.primitive(_) => Left(hydra.context.InContext(hydra.errors.Error.other("don't know how to make Java type into array type"), cx))

def javaTypeToJavaReferenceType(t: hydra.ext.java.syntax.Type)(cx: hydra.context.Context): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.java.syntax.ReferenceType] =
  t match
  case hydra.ext.java.syntax.Type.reference(v_Type_reference_rt) => Right(v_Type_reference_rt)
  case hydra.ext.java.syntax.Type.primitive(_) => Left(hydra.context.InContext(hydra.errors.Error.other("expected a Java reference type"), cx))

def javaReferenceTypeToRawType(rt: hydra.ext.java.syntax.ReferenceType): hydra.ext.java.syntax.ReferenceType =
  rt match
  case hydra.ext.java.syntax.ReferenceType.classOrInterface(v_ReferenceType_classOrInterface_cit) => v_ReferenceType_classOrInterface_cit match
    case hydra.ext.java.syntax.ClassOrInterfaceType.`class`(v_ClassOrInterfaceType_class_ct) => {
      lazy val anns: Seq[hydra.ext.java.syntax.Annotation] = (v_ClassOrInterfaceType_class_ct.annotations)
      lazy val qual: hydra.ext.java.syntax.ClassTypeQualifier = (v_ClassOrInterfaceType_class_ct.qualifier)
      lazy val id: hydra.ext.java.syntax.TypeIdentifier = (v_ClassOrInterfaceType_class_ct.identifier)
      hydra.ext.java.syntax.ReferenceType.classOrInterface(hydra.ext.java.syntax.ClassOrInterfaceType.`class`(hydra.ext.java.syntax.ClassType(anns, qual, id, Seq())))
    }
    case hydra.ext.java.syntax.ClassOrInterfaceType.interface(v_ClassOrInterfaceType_interface_it) => {
      lazy val ct: hydra.ext.java.syntax.ClassType = v_ClassOrInterfaceType_interface_it
      lazy val anns: Seq[hydra.ext.java.syntax.Annotation] = (ct.annotations)
      lazy val qual: hydra.ext.java.syntax.ClassTypeQualifier = (ct.qualifier)
      lazy val id: hydra.ext.java.syntax.TypeIdentifier = (ct.identifier)
      hydra.ext.java.syntax.ReferenceType.classOrInterface(hydra.ext.java.syntax.ClassOrInterfaceType.interface(hydra.ext.java.syntax.ClassType(anns, qual, id, Seq())))
    }
  case _ => rt

def addJavaTypeParameter(rt: hydra.ext.java.syntax.ReferenceType)(t: hydra.ext.java.syntax.Type)(cx: hydra.context.Context): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.java.syntax.Type] =
  t match
  case hydra.ext.java.syntax.Type.reference(v_Type_reference_rt1) => v_Type_reference_rt1 match
    case hydra.ext.java.syntax.ReferenceType.classOrInterface(v_ReferenceType_classOrInterface_cit) => v_ReferenceType_classOrInterface_cit match
      case hydra.ext.java.syntax.ClassOrInterfaceType.`class`(v_ClassOrInterfaceType_class_ct) => {
        lazy val anns: Seq[hydra.ext.java.syntax.Annotation] = (v_ClassOrInterfaceType_class_ct.annotations)
        lazy val qual: hydra.ext.java.syntax.ClassTypeQualifier = (v_ClassOrInterfaceType_class_ct.qualifier)
        lazy val id: hydra.ext.java.syntax.TypeIdentifier = (v_ClassOrInterfaceType_class_ct.identifier)
        lazy val args: Seq[hydra.ext.java.syntax.TypeArgument] = (v_ClassOrInterfaceType_class_ct.arguments)
        Right(hydra.ext.java.syntax.Type.reference(hydra.ext.java.syntax.ReferenceType.classOrInterface(hydra.ext.java.syntax.ClassOrInterfaceType.`class`(hydra.ext.java.syntax.ClassType(anns, qual, id, hydra.lib.lists.concat2[hydra.ext.java.syntax.TypeArgument](args)(Seq(hydra.ext.java.syntax.TypeArgument.reference(rt))))))))
      }
      case hydra.ext.java.syntax.ClassOrInterfaceType.interface(_) => Left(hydra.context.InContext(hydra.errors.Error.other("expected a Java class type"), cx))
    case hydra.ext.java.syntax.ReferenceType.variable(v_ReferenceType_variable_tv) => Right(hydra.ext.java.utils.javaTypeVariableToType(v_ReferenceType_variable_tv))
    case hydra.ext.java.syntax.ReferenceType.array(_) => Left(hydra.context.InContext(hydra.errors.Error.other("expected a Java class or interface type, or a variable"), cx))
  case hydra.ext.java.syntax.Type.primitive(_) => Left(hydra.context.InContext(hydra.errors.Error.other("expected a reference type"), cx))

def uniqueVarName(aliases: hydra.ext.java.environment.Aliases)(name: hydra.core.Name): hydra.core.Name =
  hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](name)(aliases.inScopeJavaVars))(hydra.ext.java.utils.uniqueVarName_go(aliases)(name)(2))(name)

def uniqueVarName_go(aliases: hydra.ext.java.environment.Aliases)(base: scala.Predef.String)(n: Int): hydra.core.Name =
  {
  lazy val candidate: hydra.core.Name = hydra.lib.strings.cat2(base)(hydra.lib.literals.showInt32(n))
  hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](candidate)(aliases.inScopeJavaVars))(hydra.ext.java.utils.uniqueVarName_go(aliases)(base)(hydra.lib.math.add(n)(1)))(candidate)
}

def addInScopeVar(name: hydra.core.Name)(aliases: hydra.ext.java.environment.Aliases): hydra.ext.java.environment.Aliases =
  hydra.ext.java.environment.Aliases(aliases.currentNamespace, (aliases.packages), (aliases.branchVars), (aliases.recursiveVars), (aliases.inScopeTypeParams), (aliases.polymorphicLocals), hydra.lib.sets.insert[hydra.core.Name](name)(aliases.inScopeJavaVars), (aliases.varRenames), (aliases.lambdaVars), (aliases.typeVarSubst), (aliases.trustedTypeVars), (aliases.methodCodomain), (aliases.thunkedVars))

def addInScopeVars(names: Seq[hydra.core.Name])(aliases: hydra.ext.java.environment.Aliases): hydra.ext.java.environment.Aliases =
  hydra.lib.lists.foldl[hydra.ext.java.environment.Aliases, hydra.core.Name]((a: hydra.ext.java.environment.Aliases) =>
  (n: hydra.core.Name) => hydra.ext.java.utils.addInScopeVar(n)(a))(aliases)(names)

def addVarRename(original: hydra.core.Name)(renamed: hydra.core.Name)(aliases: hydra.ext.java.environment.Aliases): hydra.ext.java.environment.Aliases =
  hydra.ext.java.environment.Aliases(aliases.currentNamespace, (aliases.packages), (aliases.branchVars), (aliases.recursiveVars), (aliases.inScopeTypeParams), (aliases.polymorphicLocals), (aliases.inScopeJavaVars), hydra.lib.maps.insert[hydra.core.Name, hydra.core.Name](original)(renamed)(aliases.varRenames), (aliases.lambdaVars), (aliases.typeVarSubst), (aliases.trustedTypeVars), (aliases.methodCodomain), (aliases.thunkedVars))
