// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An infix application expression
 */
public class InfixApplicationExpression implements Serializable, Comparable<InfixApplicationExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.InfixApplicationExpression");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  /**
   * The left-hand operand
   */
  public final hydra.ext.haskell.ast.Expression lhs;
  
  /**
   * The infix operator
   */
  public final hydra.ext.haskell.ast.Operator operator;
  
  /**
   * The right-hand operand
   */
  public final hydra.ext.haskell.ast.Expression rhs;
  
  public InfixApplicationExpression (hydra.ext.haskell.ast.Expression lhs, hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression rhs) {
    this.lhs = lhs;
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InfixApplicationExpression)) {
      return false;
    }
    InfixApplicationExpression o = (InfixApplicationExpression) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(operator) + 5 * java.util.Objects.hashCode(rhs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InfixApplicationExpression other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) operator).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public InfixApplicationExpression withLhs(hydra.ext.haskell.ast.Expression lhs) {
    return new InfixApplicationExpression(lhs, operator, rhs);
  }
  
  public InfixApplicationExpression withOperator(hydra.ext.haskell.ast.Operator operator) {
    return new InfixApplicationExpression(lhs, operator, rhs);
  }
  
  public InfixApplicationExpression withRhs(hydra.ext.haskell.ast.Expression rhs) {
    return new InfixApplicationExpression(lhs, operator, rhs);
  }
}
