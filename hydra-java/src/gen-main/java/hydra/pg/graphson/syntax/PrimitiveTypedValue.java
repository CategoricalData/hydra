// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class PrimitiveTypedValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.graphson.syntax.PrimitiveTypedValue");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.pg.graphson.syntax.TypeName type;
  
  public final String value;
  
  public PrimitiveTypedValue (hydra.pg.graphson.syntax.TypeName type, String value) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((value));
    this.type = type;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimitiveTypedValue)) {
      return false;
    }
    PrimitiveTypedValue o = (PrimitiveTypedValue) (other);
    return type.equals(o.type) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * value.hashCode();
  }
  
  public PrimitiveTypedValue withType(hydra.pg.graphson.syntax.TypeName type) {
    java.util.Objects.requireNonNull((type));
    return new PrimitiveTypedValue(type, value);
  }
  
  public PrimitiveTypedValue withValue(String value) {
    java.util.Objects.requireNonNull((value));
    return new PrimitiveTypedValue(type, value);
  }
}