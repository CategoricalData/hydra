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
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((field));
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
    java.util.Objects.requireNonNull((typeName));
    return new Projection(typeName, field);
  }
  
  public Projection withField(hydra.core.FieldName field) {
    java.util.Objects.requireNonNull((field));
    return new Projection(typeName, field);
  }
}