// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A name/type pair
 */
public class FieldType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/core.FieldType");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.core.Name name;
  
  public final hydra.core.Type type;
  
  public FieldType (hydra.core.Name name, hydra.core.Type type) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((type));
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldType)) {
      return false;
    }
    FieldType o = (FieldType) (other);
    return name.equals(o.name) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode();
  }
  
  public FieldType withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new FieldType(name, type);
  }
  
  public FieldType withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new FieldType(name, type);
  }
}