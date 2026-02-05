// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A name/type pair
 */
public class FieldType implements Serializable, Comparable<FieldType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.FieldType");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The name of the field
   */
  public final hydra.core.Name name;
  
  /**
   * The type of the field
   */
  public final hydra.core.Type type;
  
  public FieldType (hydra.core.Name name, hydra.core.Type type) {
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldType)) {
      return false;
    }
    FieldType o = (FieldType) (other);
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FieldType other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (type)).compareTo(other.type);
  }
  
  public FieldType withName(hydra.core.Name name) {
    return new FieldType(name, type);
  }
  
  public FieldType withType(hydra.core.Type type) {
    return new FieldType(name, type);
  }
}
