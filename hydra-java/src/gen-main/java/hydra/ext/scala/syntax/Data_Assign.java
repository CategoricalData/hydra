// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Assign implements Serializable, Comparable<Data_Assign> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Assign");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.ext.scala.syntax.Data lhs;

  public final hydra.ext.scala.syntax.Data rhs;

  public Data_Assign (hydra.ext.scala.syntax.Data lhs, hydra.ext.scala.syntax.Data rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Assign)) {
      return false;
    }
    Data_Assign o = (Data_Assign) other;
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
  public int compareTo(Data_Assign other) {
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

  public Data_Assign withLhs(hydra.ext.scala.syntax.Data lhs) {
    return new Data_Assign(lhs, rhs);
  }

  public Data_Assign withRhs(hydra.ext.scala.syntax.Data rhs) {
    return new Data_Assign(lhs, rhs);
  }
}
