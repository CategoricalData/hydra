// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Alternative implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Pat.Alternative");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.scala.meta.Pat lhs;
  
  public final hydra.ext.scala.meta.Pat rhs;
  
  public Pat_Alternative (hydra.ext.scala.meta.Pat lhs, hydra.ext.scala.meta.Pat rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Alternative)) {
      return false;
    }
    Pat_Alternative o = (Pat_Alternative) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Pat_Alternative withLhs(hydra.ext.scala.meta.Pat lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Pat_Alternative(lhs, rhs);
  }
  
  public Pat_Alternative withRhs(hydra.ext.scala.meta.Pat rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Pat_Alternative(lhs, rhs);
  }
}
