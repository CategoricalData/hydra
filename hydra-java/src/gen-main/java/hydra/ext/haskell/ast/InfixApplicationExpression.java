// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An infix application expression
 */
public class InfixApplicationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.InfixApplicationExpression");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.haskell.ast.Expression lhs;
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Expression rhs;
  
  public InfixApplicationExpression (hydra.ext.haskell.ast.Expression lhs, hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InfixApplicationExpression)) {
      return false;
    }
    InfixApplicationExpression o = (InfixApplicationExpression) (other);
    return lhs.equals(o.lhs) && operator.equals(o.operator) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * operator.hashCode() + 5 * rhs.hashCode();
  }
  
  public InfixApplicationExpression withLhs(hydra.ext.haskell.ast.Expression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new InfixApplicationExpression(lhs, operator, rhs);
  }
  
  public InfixApplicationExpression withOperator(hydra.ext.haskell.ast.Operator operator) {
    java.util.Objects.requireNonNull((operator));
    return new InfixApplicationExpression(lhs, operator, rhs);
  }
  
  public InfixApplicationExpression withRhs(hydra.ext.haskell.ast.Expression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new InfixApplicationExpression(lhs, operator, rhs);
  }
}