// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Type_With implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Type.With");
  
  public final hydra.langs.scala.meta.Type lhs;
  
  public final hydra.langs.scala.meta.Type rhs;
  
  public Type_With (hydra.langs.scala.meta.Type lhs, hydra.langs.scala.meta.Type rhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_With)) {
      return false;
    }
    Type_With o = (Type_With) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Type_With withLhs(hydra.langs.scala.meta.Type lhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    return new Type_With(lhs, rhs);
  }
  
  public Type_With withRhs(hydra.langs.scala.meta.Type rhs) {
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    return new Type_With(lhs, rhs);
  }
}