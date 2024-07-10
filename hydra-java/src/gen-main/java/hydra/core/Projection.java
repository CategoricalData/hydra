// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A record elimination; a projection
 */
public class Projection implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Projection");
  
  public final hydra.core.Name typeName;
  
  public final hydra.core.FieldName field;
  
  public Projection (hydra.core.Name typeName, hydra.core.FieldName field) {
    if (typeName == null) {
      throw new IllegalArgumentException("null value for 'typeName' argument");
    }
    if (field == null) {
      throw new IllegalArgumentException("null value for 'field' argument");
    }
    this.typeName = typeName;
    this.field = field;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projection)) {
      return false;
    }
    Projection o = (Projection) (other);
    return typeName.equals(o.typeName) && field.equals(o.field);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * field.hashCode();
  }
  
  public Projection withTypeName(hydra.core.Name typeName) {
    if (typeName == null) {
      throw new IllegalArgumentException("null value for 'typeName' argument");
    }
    return new Projection(typeName, field);
  }
  
  public Projection withField(hydra.core.FieldName field) {
    if (field == null) {
      throw new IllegalArgumentException("null value for 'field' argument");
    }
    return new Projection(typeName, field);
  }
}