// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type wrapped in a type name
 */
public class WrappedType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.WrappedType");
  
  public final hydra.core.Name typeName;
  
  public final hydra.core.Type object;
  
  public WrappedType (hydra.core.Name typeName, hydra.core.Type object) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((object));
    this.typeName = typeName;
    this.object = object;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WrappedType)) {
      return false;
    }
    WrappedType o = (WrappedType) (other);
    return typeName.equals(o.typeName) && object.equals(o.object);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * object.hashCode();
  }
  
  public WrappedType withTypeName(hydra.core.Name typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new WrappedType(typeName, object);
  }
  
  public WrappedType withObject(hydra.core.Type object) {
    java.util.Objects.requireNonNull((object));
    return new WrappedType(typeName, object);
  }
}