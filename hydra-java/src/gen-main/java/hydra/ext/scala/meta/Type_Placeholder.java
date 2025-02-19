// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Placeholder implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Type_Placeholder");
  
  public static final hydra.core.Name FIELD_NAME_BOUNDS = new hydra.core.Name("bounds");
  
  public final hydra.ext.scala.meta.TypeBounds bounds;
  
  public Type_Placeholder (hydra.ext.scala.meta.TypeBounds bounds) {
    java.util.Objects.requireNonNull((bounds));
    this.bounds = bounds;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Placeholder)) {
      return false;
    }
    Type_Placeholder o = (Type_Placeholder) (other);
    return bounds.equals(o.bounds);
  }
  
  @Override
  public int hashCode() {
    return 2 * bounds.hashCode();
  }
}