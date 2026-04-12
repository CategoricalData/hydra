// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Type_And implements Serializable, Comparable<Type_And> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Type_And");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.ext.scala.syntax.Type lhs;

  public final hydra.ext.scala.syntax.Type rhs;

  public Type_And (hydra.ext.scala.syntax.Type lhs, hydra.ext.scala.syntax.Type rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_And)) {
      return false;
    }
    Type_And o = (Type_And) other;
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
  public int compareTo(Type_And other) {
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

  public Type_And withLhs(hydra.ext.scala.syntax.Type lhs) {
    return new Type_And(lhs, rhs);
  }

  public Type_And withRhs(hydra.ext.scala.syntax.Type rhs) {
    return new Type_And(lhs, rhs);
  }
}
