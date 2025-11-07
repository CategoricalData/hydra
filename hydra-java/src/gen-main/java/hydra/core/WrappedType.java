// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type wrapped in a type name; a newtype
 */
public class WrappedType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.WrappedType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  /**
   * The name of the wrapper (newtype)
   */
  public final hydra.core.Name typeName;
  
  /**
   * The wrapped type
   */
  public final hydra.core.Type body;
  
  public WrappedType (hydra.core.Name typeName, hydra.core.Type body) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((body));
    this.typeName = typeName;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WrappedType)) {
      return false;
    }
    WrappedType o = (WrappedType) (other);
    return typeName.equals(o.typeName) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * body.hashCode();
  }
  
  public WrappedType withTypeName(hydra.core.Name typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new WrappedType(typeName, body);
  }
  
  public WrappedType withBody(hydra.core.Type body) {
    java.util.Objects.requireNonNull((body));
    return new WrappedType(typeName, body);
  }
}
