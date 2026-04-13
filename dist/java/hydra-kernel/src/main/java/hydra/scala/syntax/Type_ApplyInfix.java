// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_ApplyInfix implements Serializable, Comparable<Type_ApplyInfix> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_ApplyInfix");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name OP = new hydra.core.Name("op");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.scala.syntax.Type lhs;

  public final hydra.scala.syntax.Type_Name op;

  public final hydra.scala.syntax.Type rhs;

  public Type_ApplyInfix (hydra.scala.syntax.Type lhs, hydra.scala.syntax.Type_Name op, hydra.scala.syntax.Type rhs) {
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_ApplyInfix)) {
      return false;
    }
    Type_ApplyInfix o = (Type_ApplyInfix) other;
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
  public int compareTo(Type_ApplyInfix other) {
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

  public Type_ApplyInfix withLhs(hydra.scala.syntax.Type lhs) {
    return new Type_ApplyInfix(lhs, op, rhs);
  }

  public Type_ApplyInfix withOp(hydra.scala.syntax.Type_Name op) {
    return new Type_ApplyInfix(lhs, op, rhs);
  }

  public Type_ApplyInfix withRhs(hydra.scala.syntax.Type rhs) {
    return new Type_ApplyInfix(lhs, op, rhs);
  }
}
