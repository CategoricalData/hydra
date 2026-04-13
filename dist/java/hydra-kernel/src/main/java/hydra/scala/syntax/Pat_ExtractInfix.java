// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Pat_ExtractInfix implements Serializable, Comparable<Pat_ExtractInfix> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Pat_ExtractInfix");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name OP = new hydra.core.Name("op");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.scala.syntax.Pat lhs;

  public final hydra.scala.syntax.Data_Name op;

  public final java.util.List<hydra.scala.syntax.Pat> rhs;

  public Pat_ExtractInfix (hydra.scala.syntax.Pat lhs, hydra.scala.syntax.Data_Name op, java.util.List<hydra.scala.syntax.Pat> rhs) {
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_ExtractInfix)) {
      return false;
    }
    Pat_ExtractInfix o = (Pat_ExtractInfix) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.op,
      o.op) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(op) + 5 * java.util.Objects.hashCode(rhs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pat_ExtractInfix other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      lhs,
      other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      op,
      other.op);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rhs,
      other.rhs);
  }

  public Pat_ExtractInfix withLhs(hydra.scala.syntax.Pat lhs) {
    return new Pat_ExtractInfix(lhs, op, rhs);
  }

  public Pat_ExtractInfix withOp(hydra.scala.syntax.Data_Name op) {
    return new Pat_ExtractInfix(lhs, op, rhs);
  }

  public Pat_ExtractInfix withRhs(java.util.List<hydra.scala.syntax.Pat> rhs) {
    return new Pat_ExtractInfix(lhs, op, rhs);
  }
}
