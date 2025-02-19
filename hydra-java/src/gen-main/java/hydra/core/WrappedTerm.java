// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term wrapped in a type name
 */
public class WrappedTerm implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.WrappedTerm");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public final hydra.core.Name typeName;
  
  public final hydra.core.Term object;
  
  public WrappedTerm (hydra.core.Name typeName, hydra.core.Term object) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((object));
    this.typeName = typeName;
    this.object = object;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WrappedTerm)) {
      return false;
    }
    WrappedTerm o = (WrappedTerm) (other);
    return typeName.equals(o.typeName) && object.equals(o.object);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * object.hashCode();
  }
  
  public WrappedTerm withTypeName(hydra.core.Name typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new WrappedTerm(typeName, object);
  }
  
  public WrappedTerm withObject(hydra.core.Term object) {
    java.util.Objects.requireNonNull((object));
    return new WrappedTerm(typeName, object);
  }
}