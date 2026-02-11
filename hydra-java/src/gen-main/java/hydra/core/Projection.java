// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A record elimination; a projection
 */
public class Projection implements Serializable, Comparable<Projection> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Projection");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_FIELD = new hydra.core.Name("field");
  
  /**
   * The name of the record type
   */
  public final hydra.core.Name typeName;
  
  /**
   * The name of the projected field
   */
  public final hydra.core.Name field;
  
  public Projection (hydra.core.Name typeName, hydra.core.Name field) {
    this.typeName = typeName;
    this.field = field;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projection)) {
      return false;
    }
    Projection o = (Projection) other;
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.field,
      o.field);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(field);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Projection other) {
    int cmp = 0;
    cmp = ((Comparable) typeName).compareTo(other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) field).compareTo(other.field);
  }
  
  public Projection withTypeName(hydra.core.Name typeName) {
    return new Projection(typeName, field);
  }
  
  public Projection withField(hydra.core.Name field) {
    return new Projection(typeName, field);
  }
}
