// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.proto3;

import java.io.Serializable;

/**
 * The name of a field
 */
public class FieldName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.proto3.FieldName");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public FieldName (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldName)) {
      return false;
    }
    FieldName o = (FieldName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}