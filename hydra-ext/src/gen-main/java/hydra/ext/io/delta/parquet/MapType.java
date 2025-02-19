// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.delta.parquet;

import java.io.Serializable;

/**
 * Data type representing a map type.
 */
public class MapType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.delta.parquet.MapType");
  
  public static final hydra.core.Name FIELD_NAME_KEY_TYPE = new hydra.core.Name("keyType");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_TYPE = new hydra.core.Name("valueType");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_CONTAINS_NULL = new hydra.core.Name("valueContainsNull");
  
  public final hydra.ext.io.delta.parquet.DataType keyType;
  
  public final hydra.ext.io.delta.parquet.DataType valueType;
  
  public final Boolean valueContainsNull;
  
  public MapType (hydra.ext.io.delta.parquet.DataType keyType, hydra.ext.io.delta.parquet.DataType valueType, Boolean valueContainsNull) {
    java.util.Objects.requireNonNull((keyType));
    java.util.Objects.requireNonNull((valueType));
    java.util.Objects.requireNonNull((valueContainsNull));
    this.keyType = keyType;
    this.valueType = valueType;
    this.valueContainsNull = valueContainsNull;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapType)) {
      return false;
    }
    MapType o = (MapType) (other);
    return keyType.equals(o.keyType) && valueType.equals(o.valueType) && valueContainsNull.equals(o.valueContainsNull);
  }
  
  @Override
  public int hashCode() {
    return 2 * keyType.hashCode() + 3 * valueType.hashCode() + 5 * valueContainsNull.hashCode();
  }
  
  public MapType withKeyType(hydra.ext.io.delta.parquet.DataType keyType) {
    java.util.Objects.requireNonNull((keyType));
    return new MapType(keyType, valueType, valueContainsNull);
  }
  
  public MapType withValueType(hydra.ext.io.delta.parquet.DataType valueType) {
    java.util.Objects.requireNonNull((valueType));
    return new MapType(keyType, valueType, valueContainsNull);
  }
  
  public MapType withValueContainsNull(Boolean valueContainsNull) {
    java.util.Objects.requireNonNull((valueContainsNull));
    return new MapType(keyType, valueType, valueContainsNull);
  }
}