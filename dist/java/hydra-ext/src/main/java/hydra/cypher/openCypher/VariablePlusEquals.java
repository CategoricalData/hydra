// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class VariablePlusEquals implements Serializable, Comparable<VariablePlusEquals> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.VariablePlusEquals");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.cypher.openCypher.Variable lhs;

  public final hydra.cypher.openCypher.Expression rhs;

  public VariablePlusEquals (hydra.cypher.openCypher.Variable lhs, hydra.cypher.openCypher.Expression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariablePlusEquals)) {
      return false;
    }
    VariablePlusEquals o = (VariablePlusEquals) other;
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
  public int compareTo(VariablePlusEquals other) {
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

  public VariablePlusEquals withLhs(hydra.cypher.openCypher.Variable lhs) {
    return new VariablePlusEquals(lhs, rhs);
  }

  public VariablePlusEquals withRhs(hydra.cypher.openCypher.Expression rhs) {
    return new VariablePlusEquals(lhs, rhs);
  }
}
