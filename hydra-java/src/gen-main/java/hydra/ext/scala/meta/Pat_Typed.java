// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Typed implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Pat.Typed");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.scala.meta.Pat lhs;
  
  public final hydra.ext.scala.meta.Type rhs;
  
  public Pat_Typed (hydra.ext.scala.meta.Pat lhs, hydra.ext.scala.meta.Type rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Typed)) {
      return false;
    }
    Pat_Typed o = (Pat_Typed) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Pat_Typed withLhs(hydra.ext.scala.meta.Pat lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Pat_Typed(lhs, rhs);
  }
  
  public Pat_Typed withRhs(hydra.ext.scala.meta.Type rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Pat_Typed(lhs, rhs);
  }
}
