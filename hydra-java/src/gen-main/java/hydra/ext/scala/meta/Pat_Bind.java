// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Bind implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Pat_Bind");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.scala.meta.Pat lhs;
  
  public final hydra.ext.scala.meta.Pat rhs;
  
  public Pat_Bind (hydra.ext.scala.meta.Pat lhs, hydra.ext.scala.meta.Pat rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Bind)) {
      return false;
    }
    Pat_Bind o = (Pat_Bind) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Pat_Bind withLhs(hydra.ext.scala.meta.Pat lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Pat_Bind(lhs, rhs);
  }
  
  public Pat_Bind withRhs(hydra.ext.scala.meta.Pat rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Pat_Bind(lhs, rhs);
  }
}