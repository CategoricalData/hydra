// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_And implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Type_And");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.scala.meta.Type lhs;
  
  public final hydra.ext.scala.meta.Type rhs;
  
  public Type_And (hydra.ext.scala.meta.Type lhs, hydra.ext.scala.meta.Type rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_And)) {
      return false;
    }
    Type_And o = (Type_And) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Type_And withLhs(hydra.ext.scala.meta.Type lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Type_And(lhs, rhs);
  }
  
  public Type_And withRhs(hydra.ext.scala.meta.Type rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Type_And(lhs, rhs);
  }
}