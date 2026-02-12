// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.utils;

/**
 * Java utilities for constructing Java syntax trees
 */
public interface Utils {
  static hydra.ext.java.syntax.Identifier javaIdentifier(String s) {
    return new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(s));
  }
  
  static hydra.ext.java.syntax.TypeIdentifier javaTypeIdentifier(String s) {
    return new hydra.ext.java.syntax.TypeIdentifier(new hydra.ext.java.syntax.Identifier(s));
  }
  
  static hydra.ext.java.syntax.TypeName javaTypeName(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.TypeName(new hydra.ext.java.syntax.TypeIdentifier(id), (hydra.util.Maybe<hydra.ext.java.syntax.PackageOrTypeName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageOrTypeName>nothing()));
  }
  
  static hydra.ext.java.syntax.TypeIdentifier javaDeclName(hydra.core.Name name) {
    return new hydra.ext.java.syntax.TypeIdentifier(hydra.ext.java.utils.Utils.javaVariableName(name));
  }
  
  static hydra.ext.java.syntax.Identifier javaVariableName(hydra.core.Name name) {
    return hydra.ext.java.utils.Utils.javaIdentifier(hydra.names.Names.localNameOf(name));
  }
  
  static hydra.ext.java.syntax.VariableDeclaratorId javaVariableDeclaratorId(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.VariableDeclaratorId(id, (hydra.util.Maybe<hydra.ext.java.syntax.Dims>) (hydra.util.Maybe.<hydra.ext.java.syntax.Dims>nothing()));
  }
  
  static hydra.ext.java.syntax.VariableDeclarator javaVariableDeclarator(hydra.ext.java.syntax.Identifier id, hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer> minit) {
    return new hydra.ext.java.syntax.VariableDeclarator(hydra.ext.java.utils.Utils.javaVariableDeclaratorId(id), minit);
  }
  
  static hydra.ext.java.syntax.Literal javaBoolean(Boolean b) {
    return new hydra.ext.java.syntax.Literal.Boolean_(b);
  }
  
  static hydra.ext.java.syntax.Literal javaInt(java.math.BigInteger i) {
    return new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(i));
  }
  
  static hydra.ext.java.syntax.Literal javaString(String s) {
    return new hydra.ext.java.syntax.Literal.String_(new hydra.ext.java.syntax.StringLiteral(s));
  }
  
  static hydra.ext.java.syntax.Primary javaLiteralToJavaPrimary(hydra.ext.java.syntax.Literal lit) {
    return new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.Literal(lit));
  }
  
  static hydra.ext.java.syntax.Primary javaExpressionToJavaPrimary(hydra.ext.java.syntax.Expression e) {
    return new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.Parens(e));
  }
  
  static hydra.ext.java.syntax.UnaryExpression javaPrimaryToJavaUnaryExpression(hydra.ext.java.syntax.Primary p) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(p)));
  }
  
  static hydra.ext.java.syntax.Expression javaPrimaryToJavaExpression(hydra.ext.java.syntax.Primary p) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(p)))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.UnaryExpression javaPostfixExpressionToJavaUnaryExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe));
  }
  
  static hydra.ext.java.syntax.Expression javaPostfixExpressionToJavaExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.RelationalExpression javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe))))));
  }
  
  static hydra.ext.java.syntax.RelationalExpression javaUnaryExpressionToJavaRelationalExpression(hydra.ext.java.syntax.UnaryExpression ue) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(ue))));
  }
  
  static hydra.ext.java.syntax.Expression javaUnaryExpressionToJavaExpression(hydra.ext.java.syntax.UnaryExpression ue) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(ue))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.Expression javaRelationalExpressionToJavaExpression(hydra.ext.java.syntax.RelationalExpression re) {
    return hydra.ext.java.utils.Utils.javaEqualityExpressionToJavaExpression(new hydra.ext.java.syntax.EqualityExpression.Unary(re));
  }
  
  static hydra.ext.java.syntax.UnaryExpression javaRelationalExpressionToJavaUnaryExpression(hydra.ext.java.syntax.RelationalExpression re) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.Parens(new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(re)))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.RelationalExpression javaMultiplicativeExpressionToJavaRelationalExpression(hydra.ext.java.syntax.MultiplicativeExpression me) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(me)));
  }
  
  static hydra.ext.java.syntax.MultiplicativeExpression javaLiteralToJavaMultiplicativeExpression(hydra.ext.java.syntax.Literal lit) {
    return new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.Literal(lit))))));
  }
  
  static hydra.ext.java.syntax.RelationalExpression javaLiteralToJavaRelationalExpression(hydra.ext.java.syntax.Literal lit) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.Literal(lit)))))))));
  }
  
  static hydra.ext.java.syntax.Expression javaLiteralToJavaExpression(hydra.ext.java.syntax.Literal lit) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.Literal(lit)))))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.ExpressionName javaIdentifierToJavaExpressionName(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id);
  }
  
  static hydra.ext.java.syntax.Expression javaIdentifierToJavaExpression(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id))))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.RelationalExpression javaIdentifierToJavaRelationalExpression(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id))))))));
  }
  
  static hydra.ext.java.syntax.UnaryExpression javaIdentifierToJavaUnaryExpression(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id))));
  }
  
  static hydra.ext.java.syntax.Expression javaExpressionNameToJavaExpression(hydra.ext.java.syntax.ExpressionName en) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(en)))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.Expression javaFieldAccessToJavaExpression(hydra.ext.java.syntax.FieldAccess fa) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.FieldAccess(fa)))))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.Expression javaCastExpressionToJavaExpression(hydra.ext.java.syntax.CastExpression ce) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Cast(ce))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.Primary javaMethodInvocationToJavaPrimary(hydra.ext.java.syntax.MethodInvocation mi) {
    return new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.MethodInvocation(mi));
  }
  
  static hydra.ext.java.syntax.Expression javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation mi) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.MethodInvocation(mi)))))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.PostfixExpression javaMethodInvocationToJavaPostfixExpression(hydra.ext.java.syntax.MethodInvocation mi) {
    return new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.MethodInvocation(mi)));
  }
  
  static hydra.ext.java.syntax.Statement javaMethodInvocationToJavaStatement(hydra.ext.java.syntax.MethodInvocation mi) {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Expression(new hydra.ext.java.syntax.ExpressionStatement(new hydra.ext.java.syntax.StatementExpression.MethodInvocation(mi))));
  }
  
  static hydra.ext.java.syntax.Expression javaConditionalAndExpressionToJavaExpression(hydra.ext.java.syntax.ConditionalAndExpression cae) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(cae)))));
  }
  
  static hydra.ext.java.syntax.InclusiveOrExpression javaEqualityExpressionToJavaInclusiveOrExpression(hydra.ext.java.syntax.EqualityExpression ee) {
    return new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(ee))))));
  }
  
  static hydra.ext.java.syntax.Expression javaEqualityExpressionToJavaExpression(hydra.ext.java.syntax.EqualityExpression ee) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(ee)))))))))))));
  }
  
  static hydra.ext.java.syntax.EqualityExpression javaPostfixExpressionToJavaEqualityExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe)))))));
  }
  
  static hydra.ext.java.syntax.InclusiveOrExpression javaPostfixExpressionToJavaInclusiveOrExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe)))))))))))));
  }
  
  static hydra.ext.java.syntax.Expression javaAdditiveExpressionToJavaExpression(hydra.ext.java.syntax.AdditiveExpression ae) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(ae))))))))))))))));
  }
  
  static hydra.ext.java.syntax.UnaryExpression javaExpressionToJavaUnaryExpression(hydra.ext.java.syntax.Expression e) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.Parens(e)))));
  }
  
  static hydra.ext.java.syntax.Type javaPrimitiveTypeToJavaType(hydra.ext.java.syntax.PrimitiveType pt) {
    return new hydra.ext.java.syntax.Type.Primitive(new hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(pt, (java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of())));
  }
  
  static hydra.ext.java.syntax.Type javaClassTypeToJavaType(hydra.ext.java.syntax.ClassType ct) {
    return new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(ct)));
  }
  
  static hydra.ext.java.syntax.Type javaTypeVariableToType(hydra.ext.java.syntax.TypeVariable tv) {
    return new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.Variable(tv));
  }
  
  static hydra.ext.java.syntax.Type javaRefType(java.util.List<hydra.ext.java.syntax.ReferenceType> args, hydra.util.Maybe<hydra.ext.java.syntax.PackageName> pkg, String id) {
    return new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(hydra.ext.java.utils.Utils.javaClassType(
      args,
      pkg,
      id))));
  }
  
  static hydra.ext.java.syntax.ClassType javaClassType(java.util.List<hydra.ext.java.syntax.ReferenceType> args, hydra.util.Maybe<hydra.ext.java.syntax.PackageName> pkg, String id) {
    hydra.util.Lazy<hydra.ext.java.syntax.ClassTypeQualifier> qual = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
      pkg,
      new hydra.ext.java.syntax.ClassTypeQualifier.None(),
      (java.util.function.Function<hydra.ext.java.syntax.PackageName, hydra.ext.java.syntax.ClassTypeQualifier>) (p -> new hydra.ext.java.syntax.ClassTypeQualifier.Package_(p))));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.TypeArgument>> targs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.TypeArgument>) (rt -> new hydra.ext.java.syntax.TypeArgument.Reference(rt)),
      args));
    return new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), qual.get(), hydra.ext.java.utils.Utils.javaTypeIdentifier(id), targs.get());
  }
  
  static hydra.ext.java.syntax.ReferenceType javaTypeVariable(String v) {
    return new hydra.ext.java.syntax.ReferenceType.Variable(new hydra.ext.java.syntax.TypeVariable((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), hydra.ext.java.utils.Utils.javaTypeIdentifier(hydra.formatting.Formatting.capitalize(v))));
  }
  
  static hydra.ext.java.syntax.Type javaBooleanType() {
    return hydra.ext.java.utils.Utils.javaPrimitiveTypeToJavaType(new hydra.ext.java.syntax.PrimitiveType.Boolean_());
  }
  
  static hydra.ext.java.syntax.Type javaIntType() {
    return hydra.ext.java.utils.Utils.javaPrimitiveTypeToJavaType(new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Int())));
  }
  
  static hydra.ext.java.syntax.Expression javaBooleanExpression(Boolean b) {
    return hydra.ext.java.utils.Utils.javaPrimaryToJavaExpression(hydra.ext.java.utils.Utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.Utils.javaBoolean(b)));
  }
  
  static hydra.ext.java.syntax.Expression javaIntExpression(java.math.BigInteger i) {
    return hydra.ext.java.utils.Utils.javaPrimaryToJavaExpression(hydra.ext.java.utils.Utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.Utils.javaInt(i)));
  }
  
  static hydra.ext.java.syntax.CastExpression javaCastExpression(hydra.ext.java.syntax.ReferenceType rt, hydra.ext.java.syntax.UnaryExpression expr) {
    return new hydra.ext.java.syntax.CastExpression.NotPlusMinus(new hydra.ext.java.syntax.CastExpression_NotPlusMinus(new hydra.ext.java.syntax.CastExpression_RefAndBounds(rt, (java.util.List<hydra.ext.java.syntax.AdditionalBound>) (java.util.List.<hydra.ext.java.syntax.AdditionalBound>of())), expr));
  }
  
  static hydra.ext.java.syntax.CastExpression javaCastPrimitive(hydra.ext.java.syntax.PrimitiveType pt, hydra.ext.java.syntax.UnaryExpression expr) {
    return new hydra.ext.java.syntax.CastExpression.Primitive(new hydra.ext.java.syntax.CastExpression_Primitive(new hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(pt, (java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of())), expr));
  }
  
  static hydra.ext.java.syntax.Statement javaReturnStatement(hydra.util.Maybe<hydra.ext.java.syntax.Expression> mex) {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Return(new hydra.ext.java.syntax.ReturnStatement(mex)));
  }
  
  static hydra.ext.java.syntax.Statement javaThrowStatement(hydra.ext.java.syntax.Expression e) {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Throw(new hydra.ext.java.syntax.ThrowStatement(e)));
  }
  
  static hydra.ext.java.syntax.Statement javaEmptyStatement() {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Empty());
  }
  
  static hydra.ext.java.syntax.Statement javaAssignmentStatement(hydra.ext.java.syntax.LeftHandSide lhs2, hydra.ext.java.syntax.Expression rhs2) {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Expression(new hydra.ext.java.syntax.ExpressionStatement(new hydra.ext.java.syntax.StatementExpression.Assignment(new hydra.ext.java.syntax.Assignment(lhs2, new hydra.ext.java.syntax.AssignmentOperator.Simple(), rhs2)))));
  }
  
  static hydra.ext.java.syntax.Block javaStatementsToBlock(java.util.List<hydra.ext.java.syntax.Statement> stmts) {
    return new hydra.ext.java.syntax.Block(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.BlockStatement>) (s -> new hydra.ext.java.syntax.BlockStatement.Statement(s)),
      stmts));
  }
  
  static hydra.ext.java.syntax.Expression javaLambda(hydra.core.Name v, hydra.ext.java.syntax.Expression body) {
    return new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Single(hydra.ext.java.utils.Utils.variableToJavaIdentifier(v)), new hydra.ext.java.syntax.LambdaBody.Expression(body)));
  }
  
  static hydra.ext.java.syntax.Expression javaLambdaFromBlock(hydra.core.Name v, hydra.ext.java.syntax.Block block) {
    return new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Single(hydra.ext.java.utils.Utils.variableToJavaIdentifier(v)), new hydra.ext.java.syntax.LambdaBody.Block(block)));
  }
  
  static hydra.ext.java.syntax.MethodBody javaMethodBody(hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>> mstmts) {
    return hydra.lib.maybes.Cases.apply(
      mstmts,
      new hydra.ext.java.syntax.MethodBody.None(),
      (java.util.function.Function<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.syntax.MethodBody>) (stmts -> new hydra.ext.java.syntax.MethodBody.Block(new hydra.ext.java.syntax.Block(stmts))));
  }
  
  static hydra.ext.java.syntax.MethodHeader javaMethodHeader(java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, String methodName, java.util.List<hydra.ext.java.syntax.FormalParameter> params, hydra.ext.java.syntax.Result result) {
    return new hydra.ext.java.syntax.MethodHeader(tparams, result, new hydra.ext.java.syntax.MethodDeclarator(new hydra.ext.java.syntax.Identifier(methodName), (hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter>) (hydra.util.Maybe.<hydra.ext.java.syntax.ReceiverParameter>nothing()), params), (hydra.util.Maybe<hydra.ext.java.syntax.Throws>) (hydra.util.Maybe.<hydra.ext.java.syntax.Throws>nothing()));
  }
  
  static hydra.ext.java.syntax.ClassBodyDeclaration javaMethodDeclarationToJavaClassBodyDeclaration(hydra.ext.java.syntax.MethodDeclaration md) {
    return new hydra.ext.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.ext.java.syntax.ClassMemberDeclaration.Method(md));
  }
  
  static hydra.ext.java.syntax.ClassBodyDeclaration javaInterfaceDeclarationToJavaClassBodyDeclaration(hydra.ext.java.syntax.NormalInterfaceDeclaration nid) {
    return new hydra.ext.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.ext.java.syntax.ClassMemberDeclaration.Interface(new hydra.ext.java.syntax.InterfaceDeclaration.NormalInterface(nid)));
  }
  
  static hydra.ext.java.syntax.ClassBodyDeclaration javaMemberField(java.util.List<hydra.ext.java.syntax.FieldModifier> mods, hydra.ext.java.syntax.Type jt, hydra.ext.java.syntax.VariableDeclarator v) {
    return new hydra.ext.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.ext.java.syntax.ClassMemberDeclaration.Field(new hydra.ext.java.syntax.FieldDeclaration(mods, new hydra.ext.java.syntax.UnannType(jt), java.util.List.of(v))));
  }
  
  static hydra.ext.java.syntax.FormalParameter javaTypeToJavaFormalParameter(hydra.ext.java.syntax.Type jt, hydra.core.Name fname) {
    return new hydra.ext.java.syntax.FormalParameter.Simple(new hydra.ext.java.syntax.FormalParameter_Simple((java.util.List<hydra.ext.java.syntax.VariableModifier>) (java.util.List.<hydra.ext.java.syntax.VariableModifier>of()), new hydra.ext.java.syntax.UnannType(jt), hydra.ext.java.utils.Utils.fieldNameToJavaVariableDeclaratorId(fname)));
  }
  
  static hydra.ext.java.syntax.Result javaTypeToJavaResult(hydra.ext.java.syntax.Type jt) {
    return new hydra.ext.java.syntax.Result.Type(new hydra.ext.java.syntax.UnannType(jt));
  }
  
  static hydra.ext.java.syntax.TypeArgument javaTypeToJavaTypeArgument(hydra.ext.java.syntax.Type t) {
    return (t).accept(new hydra.ext.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.TypeArgument visit(hydra.ext.java.syntax.Type.Reference rt) {
        return new hydra.ext.java.syntax.TypeArgument.Reference((rt).value);
      }
      
      @Override
      public hydra.ext.java.syntax.TypeArgument visit(hydra.ext.java.syntax.Type.Primitive ignored) {
        return new hydra.ext.java.syntax.TypeArgument.Wildcard(new hydra.ext.java.syntax.Wildcard((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), (hydra.util.Maybe<hydra.ext.java.syntax.WildcardBounds>) (hydra.util.Maybe.<hydra.ext.java.syntax.WildcardBounds>nothing())));
      }
    });
  }
  
  static hydra.ext.java.syntax.Result referenceTypeToResult(hydra.ext.java.syntax.ReferenceType rt) {
    return hydra.ext.java.utils.Utils.javaTypeToJavaResult(new hydra.ext.java.syntax.Type.Reference(rt));
  }
  
  static hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate javaConstructorName(hydra.ext.java.syntax.Identifier id, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond> targs) {
    return new hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate(java.util.List.of(new hydra.ext.java.syntax.AnnotatedIdentifier((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), id)), targs);
  }
  
  static hydra.ext.java.syntax.Expression javaConstructorCall(hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate ci, java.util.List<hydra.ext.java.syntax.Expression> args, hydra.util.Maybe<hydra.ext.java.syntax.ClassBody> mbody) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.ClassInstance(new hydra.ext.java.syntax.ClassInstanceCreationExpression((hydra.util.Maybe<hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier>nothing()), new hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression((java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), ci, args, mbody)))))))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.Expression javaThis() {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.This()))))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.TypeParameter javaTypeParameter(String v) {
    return new hydra.ext.java.syntax.TypeParameter((java.util.List<hydra.ext.java.syntax.TypeParameterModifier>) (java.util.List.<hydra.ext.java.syntax.TypeParameterModifier>of()), hydra.ext.java.utils.Utils.javaTypeIdentifier(v), (hydra.util.Maybe<hydra.ext.java.syntax.TypeBound>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeBound>nothing()));
  }
  
  static hydra.ext.java.syntax.TypeArgument javaTypeIdentifierToJavaTypeArgument(hydra.ext.java.syntax.TypeIdentifier id) {
    return new hydra.ext.java.syntax.TypeArgument.Reference(new hydra.ext.java.syntax.ReferenceType.Variable(new hydra.ext.java.syntax.TypeVariable((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), id)));
  }
  
  static hydra.ext.java.syntax.TypeArgument typeParameterToTypeArgument(hydra.ext.java.syntax.TypeParameter tp) {
    return hydra.ext.java.utils.Utils.javaTypeIdentifierToJavaTypeArgument((tp).identifier);
  }
  
  static hydra.ext.java.syntax.ReferenceType typeParameterToReferenceType(hydra.ext.java.syntax.TypeParameter tp) {
    return hydra.ext.java.utils.Utils.javaTypeVariable((((tp).identifier).value).value);
  }
  
  static hydra.ext.java.syntax.Identifier fieldNameToJavaIdentifier(hydra.core.Name fname) {
    return hydra.ext.java.utils.Utils.javaIdentifier((fname).value);
  }
  
  static hydra.ext.java.syntax.Expression fieldNameToJavaExpression(hydra.core.Name fname) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.List.of(new hydra.ext.java.syntax.AndExpression(java.util.List.of(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpressionName(hydra.ext.java.utils.Utils.fieldNameToJavaIdentifier(fname)))))))))))))))))))))));
  }
  
  static hydra.ext.java.syntax.VariableDeclaratorId fieldNameToJavaVariableDeclaratorId(hydra.core.Name fname) {
    return hydra.ext.java.utils.Utils.javaVariableDeclaratorId(hydra.ext.java.utils.Utils.javaIdentifier((fname).value));
  }
  
  static hydra.ext.java.syntax.VariableDeclarator fieldNameToJavaVariableDeclarator(hydra.core.Name fname) {
    return hydra.ext.java.utils.Utils.javaVariableDeclarator(
      hydra.ext.java.utils.Utils.javaIdentifier((fname).value),
      (hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer>) (hydra.util.Maybe.<hydra.ext.java.syntax.VariableInitializer>nothing()));
  }
  
  static hydra.ext.java.syntax.ExpressionName fieldExpression(hydra.ext.java.syntax.Identifier varId, hydra.ext.java.syntax.Identifier fieldId) {
    return new hydra.ext.java.syntax.ExpressionName(hydra.util.Maybe.just(new hydra.ext.java.syntax.AmbiguousName(java.util.List.of(varId))), fieldId);
  }
  
  static hydra.ext.java.syntax.Identifier variableToJavaIdentifier(hydra.core.Name name) {
    String v = (name).value;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        v,
        "_"),
      () -> new hydra.ext.java.syntax.Identifier("ignored"),
      () -> new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(v)));
  }
  
  static hydra.ext.java.syntax.BlockStatement varDeclarationStatement(hydra.ext.java.syntax.Identifier id, hydra.ext.java.syntax.Expression rhs2) {
    return new hydra.ext.java.syntax.BlockStatement.LocalVariableDeclaration(new hydra.ext.java.syntax.LocalVariableDeclarationStatement(new hydra.ext.java.syntax.LocalVariableDeclaration((java.util.List<hydra.ext.java.syntax.VariableModifier>) (java.util.List.<hydra.ext.java.syntax.VariableModifier>of()), new hydra.ext.java.syntax.LocalVariableType.Var(), java.util.List.of(hydra.ext.java.utils.Utils.javaVariableDeclarator(
      id,
      hydra.util.Maybe.just(new hydra.ext.java.syntax.VariableInitializer.Expression(rhs2)))))));
  }
  
  static String sanitizeJavaName(String name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.java.utils.Utils.isEscaped(name),
      () -> hydra.ext.java.utils.Utils.unescape(name),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          name,
          "_"),
        () -> "ignored",
        () -> hydra.formatting.Formatting.sanitizeWithUnderscores(
          hydra.ext.java.language.Language.reservedWords(),
          name)));
  }
  
  static Boolean isEscaped(String s) {
    return hydra.lib.equality.Equal.apply(
      hydra.lib.strings.CharAt.apply(
        0,
        s),
      36);
  }
  
  static String unescape(String s) {
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.Tail.apply(hydra.lib.strings.ToList.apply(s)));
  }
  
  static hydra.ext.java.syntax.PackageDeclaration javaPackageDeclaration(hydra.module.Namespace ns) {
    return new hydra.ext.java.syntax.PackageDeclaration((java.util.List<hydra.ext.java.syntax.PackageModifier>) (java.util.List.<hydra.ext.java.syntax.PackageModifier>of()), hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.ext.java.syntax.Identifier>) (s -> new hydra.ext.java.syntax.Identifier(s)),
      hydra.lib.strings.SplitOn.apply(
        ".",
        (ns).value)));
  }
  
  static hydra.ext.java.syntax.Annotation overrideAnnotation() {
    return new hydra.ext.java.syntax.Annotation.Marker(new hydra.ext.java.syntax.MarkerAnnotation(hydra.ext.java.utils.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("Override"))));
  }
  
  static hydra.ext.java.syntax.MethodInvocation methodInvocation(hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>> lhs2, hydra.ext.java.syntax.Identifier methodName, java.util.List<hydra.ext.java.syntax.Expression> args) {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
      lhs2,
      new hydra.ext.java.syntax.MethodInvocation_Header.Simple(new hydra.ext.java.syntax.MethodName(methodName)),
      (java.util.function.Function<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>, hydra.ext.java.syntax.MethodInvocation_Header>) (either -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.MethodInvocation_Variant>) (en -> new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(en)),
        (java.util.function.Function<hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.MethodInvocation_Variant>) (p -> new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(p)),
        either), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()), methodName)))));
    return new hydra.ext.java.syntax.MethodInvocation(header.get(), args);
  }
  
  static hydra.ext.java.syntax.MethodInvocation methodInvocationStatic(hydra.ext.java.syntax.Identifier self, hydra.ext.java.syntax.Identifier methodName, java.util.List<hydra.ext.java.syntax.Expression> args) {
    return hydra.ext.java.utils.Utils.methodInvocation(
      hydra.util.Maybe.just((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) ((hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>) (hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>left(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpressionName(self))))),
      methodName,
      args);
  }
  
  static hydra.ext.java.syntax.ClassBodyDeclaration methodDeclaration(java.util.List<hydra.ext.java.syntax.MethodModifier> mods, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, java.util.List<hydra.ext.java.syntax.Annotation> anns, String methodName, java.util.List<hydra.ext.java.syntax.FormalParameter> params, hydra.ext.java.syntax.Result result, hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>> stmts) {
    return hydra.ext.java.utils.Utils.javaMethodDeclarationToJavaClassBodyDeclaration(new hydra.ext.java.syntax.MethodDeclaration(anns, mods, hydra.ext.java.utils.Utils.javaMethodHeader(
      tparams,
      methodName,
      params,
      result), hydra.ext.java.utils.Utils.javaMethodBody(stmts)));
  }
  
  static hydra.ext.java.syntax.InterfaceMemberDeclaration interfaceMethodDeclaration(java.util.List<hydra.ext.java.syntax.InterfaceMethodModifier> mods, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, String methodName, java.util.List<hydra.ext.java.syntax.FormalParameter> params, hydra.ext.java.syntax.Result result, hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>> stmts) {
    return new hydra.ext.java.syntax.InterfaceMemberDeclaration.InterfaceMethod(new hydra.ext.java.syntax.InterfaceMethodDeclaration(mods, hydra.ext.java.utils.Utils.javaMethodHeader(
      tparams,
      methodName,
      params,
      result), hydra.ext.java.utils.Utils.javaMethodBody(stmts)));
  }
  
  static hydra.ext.java.syntax.EqualityExpression javaEquals(hydra.ext.java.syntax.EqualityExpression lhs2, hydra.ext.java.syntax.RelationalExpression rhs2) {
    return new hydra.ext.java.syntax.EqualityExpression.Equal(new hydra.ext.java.syntax.EqualityExpression_Binary(lhs2, rhs2));
  }
  
  static hydra.ext.java.syntax.EqualityExpression javaEqualsNull(hydra.ext.java.syntax.EqualityExpression lhs2) {
    return hydra.ext.java.utils.Utils.javaEquals(
      lhs2,
      hydra.ext.java.utils.Utils.javaLiteralToJavaRelationalExpression(new hydra.ext.java.syntax.Literal.Null()));
  }
  
  static hydra.ext.java.syntax.RelationalExpression javaInstanceOf(hydra.ext.java.syntax.RelationalExpression lhs2, hydra.ext.java.syntax.ReferenceType rhs2) {
    return new hydra.ext.java.syntax.RelationalExpression.Instanceof(new hydra.ext.java.syntax.RelationalExpression_InstanceOf(lhs2, rhs2));
  }
  
  static hydra.ext.java.syntax.Statement javaThrowIllegalArgumentException(java.util.List<hydra.ext.java.syntax.Expression> args) {
    return hydra.ext.java.utils.Utils.javaThrowStatement(hydra.ext.java.utils.Utils.javaConstructorCall(
      hydra.ext.java.utils.Utils.javaConstructorName(
        new hydra.ext.java.syntax.Identifier("IllegalArgumentException"),
        (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
      args,
      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())));
  }
  
  static hydra.ext.java.syntax.Statement javaThrowIllegalStateException(java.util.List<hydra.ext.java.syntax.Expression> args) {
    return hydra.ext.java.utils.Utils.javaThrowStatement(hydra.ext.java.utils.Utils.javaConstructorCall(
      hydra.ext.java.utils.Utils.javaConstructorName(
        new hydra.ext.java.syntax.Identifier("IllegalStateException"),
        (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
      args,
      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())));
  }
  
  static hydra.ext.java.syntax.AdditiveExpression addExpressions(java.util.List<hydra.ext.java.syntax.MultiplicativeExpression> exprs) {
    hydra.util.Lazy<hydra.ext.java.syntax.AdditiveExpression> first = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.AdditiveExpression.Unary(hydra.lib.lists.Head.apply(exprs)));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.MultiplicativeExpression>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(exprs));
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.java.syntax.AdditiveExpression, java.util.function.Function<hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.AdditiveExpression>>) (ae -> (java.util.function.Function<hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.AdditiveExpression>) (me -> new hydra.ext.java.syntax.AdditiveExpression.Plus(new hydra.ext.java.syntax.AdditiveExpression_Binary(ae, me)))),
      first.get(),
      rest.get());
  }
  
  static hydra.ext.java.syntax.EqualityExpression javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.syntax.RelationalExpression re) {
    return new hydra.ext.java.syntax.EqualityExpression.Unary(re);
  }
  
  static hydra.util.Tuple.Tuple2<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier> nameToQualifiedJavaName(hydra.ext.java.helpers.Aliases aliases, Boolean qualify, hydra.core.Name name, hydra.util.Maybe<String> mlocal) {
    hydra.module.QualifiedName qn = hydra.names.Names.qualifyName(name);
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ext.java.syntax.PackageName>> alias = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
      ns_,
      (hydra.util.Maybe<hydra.ext.java.syntax.PackageName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageName>nothing()),
      (java.util.function.Function<hydra.module.Namespace, hydra.util.Maybe<hydra.ext.java.syntax.PackageName>>) (n -> hydra.util.Maybe.just(hydra.lib.maybes.Cases.apply(
        hydra.lib.maps.Lookup.apply(
          n,
          (aliases).packages),
        hydra.ext.java.names.Names.javaPackageName(hydra.lib.strings.SplitOn.apply(
          ".",
          (n).value)),
        (java.util.function.Function<hydra.ext.java.syntax.PackageName, hydra.ext.java.syntax.PackageName>) (id -> id))))));
    String local = (qn).local;
    hydra.util.Lazy<hydra.ext.java.syntax.TypeIdentifier> jid = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaTypeIdentifier(hydra.lib.maybes.Cases.apply(
      mlocal,
      hydra.ext.java.utils.Utils.sanitizeJavaName(local),
      (java.util.function.Function<String, String>) (l -> hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.ext.java.utils.Utils.sanitizeJavaName(local),
          "."),
        hydra.ext.java.utils.Utils.sanitizeJavaName(l))))));
    hydra.util.Lazy<hydra.ext.java.syntax.ClassTypeQualifier> pkg = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      qualify,
      () -> hydra.lib.maybes.Cases.apply(
        alias.get(),
        new hydra.ext.java.syntax.ClassTypeQualifier.None(),
        (java.util.function.Function<hydra.ext.java.syntax.PackageName, hydra.ext.java.syntax.ClassTypeQualifier>) (p -> new hydra.ext.java.syntax.ClassTypeQualifier.Package_(p))),
      () -> new hydra.ext.java.syntax.ClassTypeQualifier.None()));
    return (hydra.util.Tuple.Tuple2<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier>) ((hydra.util.Tuple.Tuple2<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier>) (new hydra.util.Tuple.Tuple2<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier>(jid.get(), pkg.get())));
  }
  
  static hydra.ext.java.syntax.ClassType nameToJavaClassType(hydra.ext.java.helpers.Aliases aliases, Boolean qualify, java.util.List<hydra.ext.java.syntax.TypeArgument> args, hydra.core.Name name, hydra.util.Maybe<String> mlocal) {
    hydra.util.Tuple.Tuple2<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier> result = hydra.ext.java.utils.Utils.nameToQualifiedJavaName(
      aliases,
      qualify,
      name,
      mlocal);
    hydra.util.Lazy<hydra.ext.java.syntax.TypeIdentifier> id = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
    hydra.util.Lazy<hydra.ext.java.syntax.ClassTypeQualifier> pkg = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
    return new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), pkg.get(), id.get(), args);
  }
  
  static hydra.ext.java.syntax.ReferenceType nameToJavaReferenceType(hydra.ext.java.helpers.Aliases aliases, Boolean qualify, java.util.List<hydra.ext.java.syntax.TypeArgument> args, hydra.core.Name name, hydra.util.Maybe<String> mlocal) {
    return new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(hydra.ext.java.utils.Utils.nameToJavaClassType(
      aliases,
      qualify,
      args,
      name,
      mlocal)));
  }
  
  static hydra.ext.java.syntax.Identifier nameToJavaName(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name name) {
    hydra.module.QualifiedName qn = hydra.names.Names.qualifyName(name);
    String local = (qn).local;
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.java.utils.Utils.isEscaped((name).value),
      () -> new hydra.ext.java.syntax.Identifier(hydra.ext.java.utils.Utils.sanitizeJavaName(local)),
      () -> hydra.lib.maybes.Cases.apply(
        ns_,
        new hydra.ext.java.syntax.Identifier(local),
        (java.util.function.Function<hydra.module.Namespace, hydra.ext.java.syntax.Identifier>) (gname -> {
          hydra.util.Lazy<java.util.List<String>> parts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
            hydra.lib.maps.Lookup.apply(
              gname,
              (aliases).packages),
            hydra.lib.strings.SplitOn.apply(
              ".",
              (gname).value),
            (java.util.function.Function<hydra.ext.java.syntax.PackageName, java.util.List<String>>) (pkgName -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.ext.java.syntax.Identifier, String>) (i -> (i).value),
              (pkgName).value))));
          hydra.util.Lazy<java.util.List<String>> allParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
            parts.get(),
            java.util.List.of(hydra.ext.java.utils.Utils.sanitizeJavaName(local))));
          return new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Intercalate.apply(
            ".",
            allParts.get()));
        })));
  }
  
  static hydra.ext.java.syntax.TypeIdentifier nameToJavaTypeIdentifier(hydra.ext.java.helpers.Aliases aliases, Boolean qualify, hydra.core.Name name) {
    return hydra.lib.pairs.First.apply(hydra.ext.java.utils.Utils.nameToQualifiedJavaName(
      aliases,
      qualify,
      name,
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
  }
  
  static hydra.ext.java.syntax.Type javaTypeFromTypeName(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name elName) {
    return hydra.ext.java.utils.Utils.javaTypeVariableToType(new hydra.ext.java.syntax.TypeVariable((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), hydra.ext.java.utils.Utils.nameToJavaTypeIdentifier(
      aliases,
      false,
      elName)));
  }
  
  static hydra.ext.java.syntax.CastExpression javaDoubleCastExpression(hydra.ext.java.syntax.ReferenceType rawRt, hydra.ext.java.syntax.ReferenceType targetRt, hydra.ext.java.syntax.UnaryExpression expr) {
    hydra.ext.java.syntax.Expression firstCast = hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaCastExpression(
      rawRt,
      expr));
    return hydra.ext.java.utils.Utils.javaCastExpression(
      targetRt,
      hydra.ext.java.utils.Utils.javaExpressionToJavaUnaryExpression(firstCast));
  }
  
  static hydra.ext.java.syntax.Expression javaDoubleCastExpressionToJavaExpression(hydra.ext.java.syntax.ReferenceType rawRt, hydra.ext.java.syntax.ReferenceType targetRt, hydra.ext.java.syntax.UnaryExpression expr) {
    return hydra.ext.java.utils.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.Utils.javaDoubleCastExpression(
      rawRt,
      targetRt,
      expr));
  }
  
  static hydra.ext.java.syntax.PrimitiveTypeWithAnnotations javaBytePrimitiveType() {
    return new hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Byte_())), (java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()));
  }
  
  static hydra.ext.java.syntax.ReferenceType visitorTypeVariable() {
    return hydra.ext.java.utils.Utils.javaTypeVariable("r");
  }
  
  static hydra.core.Name lookupJavaVarName(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name name) {
    return hydra.lib.maybes.Cases.apply(
      hydra.lib.maps.Lookup.apply(
        name,
        (aliases).varRenames),
      name,
      (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (renamed -> renamed));
  }
  
  static hydra.core.Name variantClassName(Boolean qualify, hydra.core.Name elName, hydra.core.Name fname) {
    String flocal = hydra.formatting.Formatting.capitalize((fname).value);
    hydra.module.QualifiedName qn = hydra.names.Names.qualifyName(elName);
    String local = (qn).local;
    hydra.util.Lazy<String> local1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      qualify,
      () -> hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          local,
          "."),
        flocal),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          flocal,
          local),
        () -> hydra.lib.strings.Cat2.apply(
          flocal,
          "_"),
        () -> flocal)));
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    return hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(ns_, local1.get()));
  }
  
  static <T0> hydra.ext.java.syntax.BlockStatement variableDeclarationStatement(T0 aliases, hydra.ext.java.syntax.Type jtype, hydra.ext.java.syntax.Identifier id, hydra.ext.java.syntax.Expression rhs2) {
    hydra.ext.java.syntax.VariableInitializer init_ = new hydra.ext.java.syntax.VariableInitializer.Expression(rhs2);
    hydra.ext.java.syntax.VariableDeclarator vdec = hydra.ext.java.utils.Utils.javaVariableDeclarator(
      id,
      hydra.util.Maybe.just(init_));
    return new hydra.ext.java.syntax.BlockStatement.LocalVariableDeclaration(new hydra.ext.java.syntax.LocalVariableDeclarationStatement(new hydra.ext.java.syntax.LocalVariableDeclaration((java.util.List<hydra.ext.java.syntax.VariableModifier>) (java.util.List.<hydra.ext.java.syntax.VariableModifier>of()), new hydra.ext.java.syntax.LocalVariableType.Type(new hydra.ext.java.syntax.UnannType(jtype)), java.util.List.of(vdec))));
  }
  
  static hydra.ext.java.syntax.MultiplicativeExpression javaStringMultiplicativeExpression(String s) {
    return hydra.ext.java.utils.Utils.javaLiteralToJavaMultiplicativeExpression(hydra.ext.java.utils.Utils.javaString(s));
  }
  
  static hydra.ext.java.syntax.Annotation suppressWarningsUncheckedAnnotation() {
    return new hydra.ext.java.syntax.Annotation.SingleElement(new hydra.ext.java.syntax.SingleElementAnnotation(hydra.ext.java.utils.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("SuppressWarnings")), hydra.util.Maybe.just(new hydra.ext.java.syntax.ElementValue.ConditionalExpression(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.List.of(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.List.of(hydra.ext.java.utils.Utils.javaPostfixExpressionToJavaInclusiveOrExpression(new hydra.ext.java.syntax.PostfixExpression.Primary(hydra.ext.java.utils.Utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.Utils.javaString("unchecked")))))))))))));
  }
  
  static hydra.ext.java.syntax.MethodInvocation methodInvocationStaticWithTypeArgs(hydra.ext.java.syntax.Identifier self, hydra.ext.java.syntax.Identifier methodName, java.util.List<hydra.ext.java.syntax.TypeArgument> targs, java.util.List<hydra.ext.java.syntax.Expression> args) {
    hydra.ext.java.syntax.MethodInvocation_Header header = new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(hydra.ext.java.utils.Utils.javaIdentifierToJavaExpressionName(self)), targs, methodName));
    return new hydra.ext.java.syntax.MethodInvocation(header, args);
  }
  
  static hydra.ext.java.syntax.Expression javaArrayCreation(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations primType, hydra.util.Maybe<hydra.ext.java.syntax.ArrayInitializer> minit) {
    hydra.util.Lazy<hydra.ext.java.syntax.ArrayInitializer> init_ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.apply(
      minit,
      new hydra.ext.java.syntax.ArrayInitializer((java.util.List<java.util.List<hydra.ext.java.syntax.VariableInitializer>>) (java.util.List.<java.util.List<hydra.ext.java.syntax.VariableInitializer>>of())),
      (java.util.function.Function<hydra.ext.java.syntax.ArrayInitializer, hydra.ext.java.syntax.ArrayInitializer>) (i -> i)));
    return hydra.ext.java.utils.Utils.javaPrimaryToJavaExpression(new hydra.ext.java.syntax.Primary.ArrayCreation(new hydra.ext.java.syntax.ArrayCreationExpression.PrimitiveArray(new hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray(primType, (java.util.List<hydra.ext.java.syntax.Dims>) (java.util.List.<hydra.ext.java.syntax.Dims>of()), init_.get()))));
  }
  
  static hydra.ext.java.syntax.ArrayInitializer javaArrayInitializer(java.util.List<hydra.ext.java.syntax.Expression> exprs) {
    return new hydra.ext.java.syntax.ArrayInitializer(java.util.List.of(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.VariableInitializer>) (e -> new hydra.ext.java.syntax.VariableInitializer.Expression(e)),
      exprs)));
  }
  
  static hydra.ext.java.syntax.Statement toAssignStmt(hydra.core.Name fname) {
    hydra.ext.java.syntax.Identifier id = hydra.ext.java.utils.Utils.fieldNameToJavaIdentifier(fname);
    hydra.ext.java.syntax.LeftHandSide lhs = new hydra.ext.java.syntax.LeftHandSide.FieldAccess(new hydra.ext.java.syntax.FieldAccess(new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArray.This())), id));
    hydra.ext.java.syntax.Expression rhs = hydra.ext.java.utils.Utils.fieldNameToJavaExpression(fname);
    return hydra.ext.java.utils.Utils.javaAssignmentStatement(
      lhs,
      rhs);
  }
  
  static String unTypeParameter(hydra.ext.java.syntax.TypeParameter tp) {
    return (((tp).identifier).value).value;
  }
  
  static hydra.ext.java.helpers.Aliases importAliasesForModule(hydra.module.Module mod) {
    return new hydra.ext.java.helpers.Aliases((mod).namespace, (java.util.Map<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>) ((java.util.Map<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>) (hydra.lib.maps.Empty.<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()));
  }
  
  static hydra.ext.java.syntax.ClassDeclaration javaClassDeclaration(hydra.ext.java.helpers.Aliases aliases, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, java.util.List<hydra.ext.java.syntax.ClassModifier> mods, hydra.util.Maybe<hydra.core.Name> supname, java.util.List<hydra.ext.java.syntax.InterfaceType> impls, java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments> bodyDecls) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.ext.java.syntax.ClassType>> extends_ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.ClassType>) (n -> hydra.ext.java.utils.Utils.nameToJavaClassType(
        aliases,
        true,
        (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()),
        n,
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
      supname));
    return new hydra.ext.java.syntax.ClassDeclaration.Normal(new hydra.ext.java.syntax.NormalClassDeclaration(mods, hydra.ext.java.utils.Utils.javaDeclName(elName), tparams, extends_.get(), impls, new hydra.ext.java.syntax.ClassBody(bodyDecls)));
  }
  
  static hydra.ext.java.syntax.ClassBodyDeclaration makeConstructor(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name elName, Boolean private_, java.util.List<hydra.ext.java.syntax.FormalParameter> params, java.util.List<hydra.ext.java.syntax.BlockStatement> stmts) {
    hydra.util.Lazy<hydra.ext.java.syntax.ConstructorBody> body = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.ConstructorBody((hydra.util.Maybe<hydra.ext.java.syntax.ExplicitConstructorInvocation>) (hydra.util.Maybe.<hydra.ext.java.syntax.ExplicitConstructorInvocation>nothing()), stmts));
    hydra.ext.java.syntax.SimpleTypeName nm = new hydra.ext.java.syntax.SimpleTypeName(hydra.ext.java.utils.Utils.nameToJavaTypeIdentifier(
      aliases,
      false,
      elName));
    hydra.util.Lazy<hydra.ext.java.syntax.ConstructorDeclarator> cons = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.ConstructorDeclarator((java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.List.<hydra.ext.java.syntax.TypeParameter>of()), nm, (hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter>) (hydra.util.Maybe.<hydra.ext.java.syntax.ReceiverParameter>nothing()), params));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ConstructorModifier>> mods = new hydra.util.Lazy<>(() -> java.util.List.of(hydra.lib.logic.IfElse.lazy(
      private_,
      () -> new hydra.ext.java.syntax.ConstructorModifier.Private(),
      () -> new hydra.ext.java.syntax.ConstructorModifier.Public())));
    return new hydra.ext.java.syntax.ClassBodyDeclaration.ConstructorDeclaration(new hydra.ext.java.syntax.ConstructorDeclaration(mods.get(), cons.get(), (hydra.util.Maybe<hydra.ext.java.syntax.Throws>) (hydra.util.Maybe.<hydra.ext.java.syntax.Throws>nothing()), body.get()));
  }
  
  static hydra.ext.java.syntax.ClassBodyDeclaration toAcceptMethod(Boolean abstract_, java.util.List<hydra.ext.java.syntax.TypeParameter> vtparams) {
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.Annotation>> anns = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      abstract_,
      () -> (java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()),
      () -> java.util.List.of(hydra.ext.java.utils.Utils.overrideAnnotation())));
    hydra.ext.java.syntax.Expression returnExpr = hydra.ext.java.utils.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.Utils.methodInvocationStatic(
      new hydra.ext.java.syntax.Identifier("visitor"),
      new hydra.ext.java.syntax.Identifier(hydra.ext.java.names.Names.visitMethodName()),
      java.util.List.of(hydra.ext.java.utils.Utils.javaThis())));
    hydra.util.Lazy<hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>>> body = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      abstract_,
      () -> (hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>>) (hydra.util.Maybe.<java.util.List<hydra.ext.java.syntax.BlockStatement>>nothing()),
      () -> hydra.util.Maybe.just(java.util.List.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.utils.Utils.javaReturnStatement(hydra.util.Maybe.just(returnExpr)))))));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.MethodModifier>> mods = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      abstract_,
      () -> java.util.List.of(
        new hydra.ext.java.syntax.MethodModifier.Public(),
        new hydra.ext.java.syntax.MethodModifier.Abstract()),
      () -> java.util.List.of(new hydra.ext.java.syntax.MethodModifier.Public())));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.TypeArgument>> typeArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp -> new hydra.ext.java.syntax.TypeArgument.Reference(hydra.ext.java.utils.Utils.typeParameterToReferenceType(tp))),
      vtparams));
    hydra.util.Lazy<hydra.ext.java.syntax.Type> ref = new hydra.util.Lazy<>(() -> hydra.ext.java.utils.Utils.javaClassTypeToJavaType(new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.utils.Utils.javaTypeIdentifier(hydra.ext.java.names.Names.visitorName()), hydra.lib.lists.Concat2.apply(
      typeArgs.get(),
      java.util.List.of(new hydra.ext.java.syntax.TypeArgument.Reference(hydra.ext.java.utils.Utils.visitorTypeVariable()))))));
    hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.utils.Utils.javaTypeToJavaFormalParameter(
      ref.get(),
      new hydra.core.Name("visitor"));
    hydra.ext.java.syntax.Result result = hydra.ext.java.utils.Utils.javaTypeToJavaResult(new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.utils.Utils.visitorTypeVariable()));
    java.util.List<hydra.ext.java.syntax.TypeParameter> tparams = java.util.List.of(hydra.ext.java.utils.Utils.javaTypeParameter(hydra.ext.java.names.Names.visitorReturnParameter()));
    return hydra.ext.java.utils.Utils.methodDeclaration(
      mods.get(),
      tparams,
      anns.get(),
      hydra.ext.java.names.Names.acceptMethodName(),
      java.util.List.of(param),
      result,
      body.get());
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> toJavaArrayType(hydra.ext.java.syntax.Type t) {
    return (t).accept(new hydra.ext.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.Type.Reference rt) {
        return ((rt).value).accept(new hydra.ext.java.syntax.ReferenceType.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.ClassOrInterface cit) {
            return hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.Array(new hydra.ext.java.syntax.ArrayType(new hydra.ext.java.syntax.Dims(java.util.List.of((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of()))), new hydra.ext.java.syntax.ArrayType_Variant.ClassOrInterface((cit).value)))));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.Array at) {
            java.util.List<java.util.List<hydra.ext.java.syntax.Annotation>> oldDims = (((at).value).dims).value;
            hydra.util.Lazy<hydra.ext.java.syntax.Dims> newDims = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Dims(hydra.lib.lists.Concat2.apply(
              oldDims,
              java.util.List.of((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.List.<hydra.ext.java.syntax.Annotation>of())))));
            hydra.ext.java.syntax.ArrayType_Variant variant = ((at).value).variant;
            return hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.Array(new hydra.ext.java.syntax.ArrayType(newDims.get(), variant))));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.Variable ignored) {
            return hydra.lib.flows.Fail.apply("don't know how to make Java reference type into array type");
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.Type.Primitive ignored) {
        return hydra.lib.flows.Fail.apply("don't know how to make Java type into array type");
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.java.syntax.ReferenceType> javaTypeToJavaReferenceType(hydra.ext.java.syntax.Type t) {
    return (t).accept(new hydra.ext.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.ReferenceType> visit(hydra.ext.java.syntax.Type.Reference rt) {
        return hydra.lib.flows.Pure.apply((rt).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.ReferenceType> visit(hydra.ext.java.syntax.Type.Primitive ignored) {
        return hydra.lib.flows.Fail.apply("expected a Java reference type");
      }
    });
  }
  
  static hydra.ext.java.syntax.ReferenceType javaReferenceTypeToRawType(hydra.ext.java.syntax.ReferenceType rt) {
    return (rt).accept(new hydra.ext.java.syntax.ReferenceType.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.ReferenceType otherwise(hydra.ext.java.syntax.ReferenceType instance) {
        return rt;
      }
      
      @Override
      public hydra.ext.java.syntax.ReferenceType visit(hydra.ext.java.syntax.ReferenceType.ClassOrInterface cit) {
        return ((cit).value).accept(new hydra.ext.java.syntax.ClassOrInterfaceType.PartialVisitor<>() {
          @Override
          public hydra.ext.java.syntax.ReferenceType visit(hydra.ext.java.syntax.ClassOrInterfaceType.Class_ ct) {
            java.util.List<hydra.ext.java.syntax.Annotation> anns = ((ct).value).annotations;
            hydra.ext.java.syntax.TypeIdentifier id = ((ct).value).identifier;
            hydra.ext.java.syntax.ClassTypeQualifier qual = ((ct).value).qualifier;
            return new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(new hydra.ext.java.syntax.ClassType(anns, qual, id, (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of()))));
          }
          
          @Override
          public hydra.ext.java.syntax.ReferenceType visit(hydra.ext.java.syntax.ClassOrInterfaceType.Interface it) {
            hydra.ext.java.syntax.ClassType ct = ((it).value).value;
            java.util.List<hydra.ext.java.syntax.Annotation> anns = (ct).annotations;
            hydra.ext.java.syntax.TypeIdentifier id = (ct).identifier;
            hydra.ext.java.syntax.ClassTypeQualifier qual = (ct).qualifier;
            return new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Interface(new hydra.ext.java.syntax.InterfaceType(new hydra.ext.java.syntax.ClassType(anns, qual, id, (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.List.<hydra.ext.java.syntax.TypeArgument>of())))));
          }
        });
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> addJavaTypeParameter(hydra.ext.java.syntax.ReferenceType rt, hydra.ext.java.syntax.Type t) {
    return (t).accept(new hydra.ext.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.Type.Reference rt1) {
        return ((rt1).value).accept(new hydra.ext.java.syntax.ReferenceType.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.ClassOrInterface cit) {
            return ((cit).value).accept(new hydra.ext.java.syntax.ClassOrInterfaceType.PartialVisitor<>() {
              @Override
              public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ClassOrInterfaceType.Class_ ct) {
                java.util.List<hydra.ext.java.syntax.Annotation> anns = ((ct).value).annotations;
                java.util.List<hydra.ext.java.syntax.TypeArgument> args = ((ct).value).arguments;
                hydra.ext.java.syntax.TypeIdentifier id = ((ct).value).identifier;
                hydra.ext.java.syntax.ClassTypeQualifier qual = ((ct).value).qualifier;
                return hydra.lib.flows.Pure.apply(new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(new hydra.ext.java.syntax.ClassType(anns, qual, id, hydra.lib.lists.Concat2.apply(
                  args,
                  java.util.List.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))));
              }
              
              @Override
              public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ClassOrInterfaceType.Interface ignored) {
                return hydra.lib.flows.Fail.apply("expected a Java class type");
              }
            });
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.Variable tv) {
            return hydra.lib.flows.Pure.apply(hydra.ext.java.utils.Utils.javaTypeVariableToType((tv).value));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.Array ignored) {
            return hydra.lib.flows.Fail.apply("expected a Java class or interface type, or a variable");
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.Type.Primitive ignored) {
        return hydra.lib.flows.Fail.apply("expected a reference type");
      }
    });
  }
  
  static hydra.core.Name uniqueVarName(hydra.ext.java.helpers.Aliases aliases, hydra.core.Name name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        name,
        (aliases).inScopeJavaVars),
      () -> hydra.ext.java.utils.Utils.uniqueVarName_go(
        aliases,
        (name).value,
        2),
      () -> name);
  }
  
  static hydra.core.Name uniqueVarName_go(hydra.ext.java.helpers.Aliases aliases, String base, Integer n) {
    hydra.core.Name candidate = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      base,
      hydra.lib.literals.ShowInt32.apply(n)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        candidate,
        (aliases).inScopeJavaVars),
      () -> hydra.ext.java.utils.Utils.uniqueVarName_go(
        aliases,
        base,
        hydra.lib.math.Add.apply(
          n,
          1)),
      () -> candidate);
  }
  
  static hydra.ext.java.helpers.Aliases addInScopeVar(hydra.core.Name name, hydra.ext.java.helpers.Aliases aliases) {
    return new hydra.ext.java.helpers.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, hydra.lib.sets.Insert.apply(
      name,
      (aliases).inScopeJavaVars), (aliases).varRenames, (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars);
  }
  
  static hydra.ext.java.helpers.Aliases addInScopeVars(java.util.List<hydra.core.Name> names, hydra.ext.java.helpers.Aliases aliases) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.java.helpers.Aliases, java.util.function.Function<hydra.core.Name, hydra.ext.java.helpers.Aliases>>) (a -> (java.util.function.Function<hydra.core.Name, hydra.ext.java.helpers.Aliases>) (n -> hydra.ext.java.utils.Utils.addInScopeVar(
        n,
        a))),
      aliases,
      names);
  }
  
  static hydra.ext.java.helpers.Aliases addVarRename(hydra.core.Name original, hydra.core.Name renamed, hydra.ext.java.helpers.Aliases aliases) {
    return new hydra.ext.java.helpers.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, (aliases).inScopeJavaVars, hydra.lib.maps.Insert.apply(
      original,
      renamed,
      (aliases).varRenames), (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars);
  }
}
