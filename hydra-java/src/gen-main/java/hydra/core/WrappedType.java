// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type wrapped in a type name; a newtype
 */
public class WrappedType implements Serializable, Comparable<WrappedType> {
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
    this.typeName = typeName;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WrappedType)) {
      return false;
    }
    WrappedType o = (WrappedType) (other);
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WrappedType other) {
    int cmp = 0;
    cmp = ((Comparable) (typeName)).compareTo(other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (body)).compareTo(other.body);
  }
  
  public WrappedType withTypeName(hydra.core.Name typeName) {
    return new WrappedType(typeName, body);
  }
  
  public WrappedType withBody(hydra.core.Type body) {
    return new WrappedType(typeName, body);
  }
}
