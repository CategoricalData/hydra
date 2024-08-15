// Note: this is an automatically generated file. Do not edit.

package hydra.langs.protobuf.proto3;

import java.io.Serializable;

/**
 * A reference to an enum type or message type
 */
public class TypeReference implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/protobuf/proto3.TypeReference");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public TypeReference (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeReference)) {
      return false;
    }
    TypeReference o = (TypeReference) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}