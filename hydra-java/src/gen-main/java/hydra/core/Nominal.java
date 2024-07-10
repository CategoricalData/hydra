// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * An object wrapped in a type name
 */
public class Nominal<X> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Nominal");
  
  public final hydra.core.Name typeName;
  
  public final X object;
  
  public Nominal (hydra.core.Name typeName, X object) {
    if (typeName == null) {
      throw new IllegalArgumentException("null value for 'typeName' argument");
    }
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    this.typeName = typeName;
    this.object = object;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Nominal)) {
      return false;
    }
    Nominal o = (Nominal) (other);
    return typeName.equals(o.typeName) && object.equals(o.object);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * object.hashCode();
  }
  
  public Nominal withTypeName(hydra.core.Name typeName) {
    if (typeName == null) {
      throw new IllegalArgumentException("null value for 'typeName' argument");
    }
    return new Nominal(typeName, object);
  }
  
  public Nominal withObject(X object) {
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    return new Nominal(typeName, object);
  }
}