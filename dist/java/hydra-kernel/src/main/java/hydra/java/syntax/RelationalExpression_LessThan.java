// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class RelationalExpression_LessThan implements Serializable, Comparable<RelationalExpression_LessThan> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.RelationalExpression_LessThan");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.java.syntax.RelationalExpression lhs;

  public final hydra.java.syntax.ShiftExpression rhs;

  public RelationalExpression_LessThan (hydra.java.syntax.RelationalExpression lhs, hydra.java.syntax.ShiftExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_LessThan)) {
      return false;
    }
    RelationalExpression_LessThan o = (RelationalExpression_LessThan) other;
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
  public int compareTo(RelationalExpression_LessThan other) {
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

  public RelationalExpression_LessThan withLhs(hydra.java.syntax.RelationalExpression lhs) {
    return new RelationalExpression_LessThan(lhs, rhs);
  }

  public RelationalExpression_LessThan withRhs(hydra.java.syntax.ShiftExpression rhs) {
    return new RelationalExpression_LessThan(lhs, rhs);
  }
}
