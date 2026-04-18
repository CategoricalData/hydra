// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * An infix application expression
 */
public class InfixApplicationExpression implements Serializable, Comparable<InfixApplicationExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  /**
   * The left-hand operand
   */
  public final hydra.haskell.syntax.Expression lhs;

  /**
   * The infix operator
   */
  public final hydra.haskell.syntax.Operator operator;

  /**
   * The right-hand operand
   */
  public final hydra.haskell.syntax.Expression rhs;

  public InfixApplicationExpression (hydra.haskell.syntax.Expression lhs, hydra.haskell.syntax.Operator operator, hydra.haskell.syntax.Expression rhs) {
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
    cmp = hydra.util.Comparing.compare(
      lhs,
      other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      operator,
      other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rhs,
      other.rhs);
  }

  public InfixApplicationExpression withLhs(hydra.haskell.syntax.Expression lhs) {
    return new InfixApplicationExpression(lhs, operator, rhs);
  }

  public InfixApplicationExpression withOperator(hydra.haskell.syntax.Operator operator) {
    return new InfixApplicationExpression(lhs, operator, rhs);
  }

  public InfixApplicationExpression withRhs(hydra.haskell.syntax.Expression rhs) {
    return new InfixApplicationExpression(lhs, operator, rhs);
  }
}
