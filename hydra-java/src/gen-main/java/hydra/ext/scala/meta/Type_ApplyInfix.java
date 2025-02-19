// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_ApplyInfix implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Type_ApplyInfix");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OP = new hydra.core.Name("op");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.scala.meta.Type lhs;
  
  public final hydra.ext.scala.meta.Type_Name op;
  
  public final hydra.ext.scala.meta.Type rhs;
  
  public Type_ApplyInfix (hydra.ext.scala.meta.Type lhs, hydra.ext.scala.meta.Type_Name op, hydra.ext.scala.meta.Type rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((op));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_ApplyInfix)) {
      return false;
    }
    Type_ApplyInfix o = (Type_ApplyInfix) (other);
    return lhs.equals(o.lhs) && op.equals(o.op) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * op.hashCode() + 5 * rhs.hashCode();
  }
  
  public Type_ApplyInfix withLhs(hydra.ext.scala.meta.Type lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Type_ApplyInfix(lhs, op, rhs);
  }
  
  public Type_ApplyInfix withOp(hydra.ext.scala.meta.Type_Name op) {
    java.util.Objects.requireNonNull((op));
    return new Type_ApplyInfix(lhs, op, rhs);
  }
  
  public Type_ApplyInfix withRhs(hydra.ext.scala.meta.Type rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Type_ApplyInfix(lhs, op, rhs);
  }
}