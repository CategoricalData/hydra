// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.proto3;

import java.io.Serializable;

public class MapType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.proto3.MapType");
  
  public static final hydra.core.Name FIELD_NAME_KEYS = new hydra.core.Name("keys");
  
  public static final hydra.core.Name FIELD_NAME_VALUES = new hydra.core.Name("values");
  
  public final hydra.ext.protobuf.proto3.SimpleType keys;
  
  public final hydra.ext.protobuf.proto3.SimpleType values;
  
  public MapType (hydra.ext.protobuf.proto3.SimpleType keys, hydra.ext.protobuf.proto3.SimpleType values) {
    java.util.Objects.requireNonNull((keys));
    java.util.Objects.requireNonNull((values));
    this.keys = keys;
    this.values = values;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapType)) {
      return false;
    }
    MapType o = (MapType) (other);
    return keys.equals(o.keys) && values.equals(o.values);
  }
  
  @Override
  public int hashCode() {
    return 2 * keys.hashCode() + 3 * values.hashCode();
  }
  
  public MapType withKeys(hydra.ext.protobuf.proto3.SimpleType keys) {
    java.util.Objects.requireNonNull((keys));
    return new MapType(keys, values);
  }
  
  public MapType withValues(hydra.ext.protobuf.proto3.SimpleType values) {
    java.util.Objects.requireNonNull((values));
    return new MapType(keys, values);
  }
}