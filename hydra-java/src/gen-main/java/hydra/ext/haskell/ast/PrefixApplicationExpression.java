// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A prefix expression
 */
public class PrefixApplicationExpression implements Serializable, Comparable<PrefixApplicationExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.PrefixApplicationExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  /**
   * The prefix operator
   */
  public final hydra.ext.haskell.ast.Operator operator;
  
  /**
   * The operand
   */
  public final hydra.ext.haskell.ast.Expression rhs;
  
  public PrefixApplicationExpression (hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression rhs) {
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrefixApplicationExpression)) {
      return false;
    }
    PrefixApplicationExpression o = (PrefixApplicationExpression) (other);
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(rhs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PrefixApplicationExpression other) {
    int cmp = 0;
    cmp = ((Comparable) (operator)).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (rhs)).compareTo(other.rhs);
  }
  
  public PrefixApplicationExpression withOperator(hydra.ext.haskell.ast.Operator operator) {
    return new PrefixApplicationExpression(operator, rhs);
  }
  
  public PrefixApplicationExpression withRhs(hydra.ext.haskell.ast.Expression rhs) {
    return new PrefixApplicationExpression(operator, rhs);
  }
}
