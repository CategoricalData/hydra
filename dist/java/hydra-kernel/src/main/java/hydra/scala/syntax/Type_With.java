// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_With implements Serializable, Comparable<Type_With> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_With");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.scala.syntax.Type lhs;

  public final hydra.scala.syntax.Type rhs;

  public Type_With (hydra.scala.syntax.Type lhs, hydra.scala.syntax.Type rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_With)) {
      return false;
    }
    Type_With o = (Type_With) other;
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
  public int compareTo(Type_With other) {
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

  public Type_With withLhs(hydra.scala.syntax.Type lhs) {
    return new Type_With(lhs, rhs);
  }

  public Type_With withRhs(hydra.scala.syntax.Type rhs) {
    return new Type_With(lhs, rhs);
  }
}
