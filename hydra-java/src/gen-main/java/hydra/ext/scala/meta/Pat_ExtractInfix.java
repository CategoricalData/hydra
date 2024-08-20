// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_ExtractInfix implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Pat.ExtractInfix");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OP = new hydra.core.Name("op");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.scala.meta.Pat lhs;
  
  public final hydra.ext.scala.meta.Data_Name op;
  
  public final java.util.List<hydra.ext.scala.meta.Pat> rhs;
  
  public Pat_ExtractInfix (hydra.ext.scala.meta.Pat lhs, hydra.ext.scala.meta.Data_Name op, java.util.List<hydra.ext.scala.meta.Pat> rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((op));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_ExtractInfix)) {
      return false;
    }
    Pat_ExtractInfix o = (Pat_ExtractInfix) (other);
    return lhs.equals(o.lhs) && op.equals(o.op) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * op.hashCode() + 5 * rhs.hashCode();
  }
  
  public Pat_ExtractInfix withLhs(hydra.ext.scala.meta.Pat lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Pat_ExtractInfix(lhs, op, rhs);
  }
  
  public Pat_ExtractInfix withOp(hydra.ext.scala.meta.Data_Name op) {
    java.util.Objects.requireNonNull((op));
    return new Pat_ExtractInfix(lhs, op, rhs);
  }
  
  public Pat_ExtractInfix withRhs(java.util.List<hydra.ext.scala.meta.Pat> rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Pat_ExtractInfix(lhs, op, rhs);
  }
}
