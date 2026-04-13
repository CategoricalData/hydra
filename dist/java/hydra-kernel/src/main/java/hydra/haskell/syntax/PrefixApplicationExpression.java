// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A prefix expression
 */
public class PrefixApplicationExpression implements Serializable, Comparable<PrefixApplicationExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  /**
   * The prefix operator
   */
  public final hydra.haskell.syntax.Operator operator;

  /**
   * The operand
   */
  public final hydra.haskell.syntax.Expression rhs;

  public PrefixApplicationExpression (hydra.haskell.syntax.Operator operator, hydra.haskell.syntax.Expression rhs) {
    this.operator = operator;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrefixApplicationExpression)) {
      return false;
    }
    PrefixApplicationExpression o = (PrefixApplicationExpression) other;
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

  public PrefixApplicationExpression withOperator(hydra.haskell.syntax.Operator operator) {
    return new PrefixApplicationExpression(operator, rhs);
  }

  public PrefixApplicationExpression withRhs(hydra.haskell.syntax.Expression rhs) {
    return new PrefixApplicationExpression(operator, rhs);
  }
}
