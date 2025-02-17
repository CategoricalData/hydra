// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An infix application expression
 */
public class Expression_InfixApplication implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.Expression.InfixApplication");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.haskell.ast.Expression lhs;
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Expression rhs;
  
  public Expression_InfixApplication (hydra.ext.haskell.ast.Expression lhs, hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_InfixApplication)) {
      return false;
    }
    Expression_InfixApplication o = (Expression_InfixApplication) (other);
    return lhs.equals(o.lhs) && operator.equals(o.operator) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * operator.hashCode() + 5 * rhs.hashCode();
  }
  
  public Expression_InfixApplication withLhs(hydra.ext.haskell.ast.Expression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Expression_InfixApplication(lhs, operator, rhs);
  }
  
  public Expression_InfixApplication withOperator(hydra.ext.haskell.ast.Operator operator) {
    java.util.Objects.requireNonNull((operator));
    return new Expression_InfixApplication(lhs, operator, rhs);
  }
  
  public Expression_InfixApplication withRhs(hydra.ext.haskell.ast.Expression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Expression_InfixApplication(lhs, operator, rhs);
  }
}
