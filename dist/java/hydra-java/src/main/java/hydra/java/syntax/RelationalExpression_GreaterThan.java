// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class RelationalExpression_GreaterThan implements Serializable, Comparable<RelationalExpression_GreaterThan> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.RelationalExpression_GreaterThan");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.java.syntax.RelationalExpression lhs;

  public final hydra.java.syntax.ShiftExpression rhs;

  public RelationalExpression_GreaterThan (hydra.java.syntax.RelationalExpression lhs, hydra.java.syntax.ShiftExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_GreaterThan)) {
      return false;
    }
    RelationalExpression_GreaterThan o = (RelationalExpression_GreaterThan) other;
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
  public int compareTo(RelationalExpression_GreaterThan other) {
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

  public RelationalExpression_GreaterThan withLhs(hydra.java.syntax.RelationalExpression lhs) {
    return new RelationalExpression_GreaterThan(lhs, rhs);
  }

  public RelationalExpression_GreaterThan withRhs(hydra.java.syntax.ShiftExpression rhs) {
    return new RelationalExpression_GreaterThan(lhs, rhs);
  }
}
