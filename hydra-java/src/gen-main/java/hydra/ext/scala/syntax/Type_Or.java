// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Type_Or implements Serializable, Comparable<Type_Or> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Type_Or");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.ext.scala.syntax.Type lhs;

  public final hydra.ext.scala.syntax.Type rhs;

  public Type_Or (hydra.ext.scala.syntax.Type lhs, hydra.ext.scala.syntax.Type rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Or)) {
      return false;
    }
    Type_Or o = (Type_Or) other;
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
  public int compareTo(Type_Or other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }

  public Type_Or withLhs(hydra.ext.scala.syntax.Type lhs) {
    return new Type_Or(lhs, rhs);
  }

  public Type_Or withRhs(hydra.ext.scala.syntax.Type rhs) {
    return new Type_Or(lhs, rhs);
  }
}
