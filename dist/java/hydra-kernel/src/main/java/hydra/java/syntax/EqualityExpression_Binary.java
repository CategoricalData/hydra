// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class EqualityExpression_Binary implements Serializable, Comparable<EqualityExpression_Binary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.EqualityExpression_Binary");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.java.syntax.EqualityExpression lhs;

  public final hydra.java.syntax.RelationalExpression rhs;

  public EqualityExpression_Binary (hydra.java.syntax.EqualityExpression lhs, hydra.java.syntax.RelationalExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EqualityExpression_Binary)) {
      return false;
    }
    EqualityExpression_Binary o = (EqualityExpression_Binary) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(rhs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EqualityExpression_Binary other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      lhs,
      other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rhs,
      other.rhs);
  }

  public EqualityExpression_Binary withLhs(hydra.java.syntax.EqualityExpression lhs) {
    return new EqualityExpression_Binary(lhs, rhs);
  }

  public EqualityExpression_Binary withRhs(hydra.java.syntax.RelationalExpression rhs) {
    return new EqualityExpression_Binary(lhs, rhs);
  }
}
