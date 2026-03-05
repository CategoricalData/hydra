// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.delta.parquet;

import java.io.Serializable;

/**
 * Data type representing a map type.
 */
public class MapType implements Serializable, Comparable<MapType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.delta.parquet.MapType");
  
  public static final hydra.core.Name KEY_TYPE = new hydra.core.Name("keyType");
  
  public static final hydra.core.Name VALUE_TYPE = new hydra.core.Name("valueType");
  
  public static final hydra.core.Name VALUE_CONTAINS_NULL = new hydra.core.Name("valueContainsNull");
  
  public final hydra.ext.io.delta.parquet.DataType keyType;
  
  public final hydra.ext.io.delta.parquet.DataType valueType;
  
  public final Boolean valueContainsNull;
  
  public MapType (hydra.ext.io.delta.parquet.DataType keyType, hydra.ext.io.delta.parquet.DataType valueType, Boolean valueContainsNull) {
    this.keyType = keyType;
    this.valueType = valueType;
    this.valueContainsNull = valueContainsNull;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapType)) {
      return false;
    }
    MapType o = (MapType) other;
    return java.util.Objects.equals(
      this.keyType,
      o.keyType) && java.util.Objects.equals(
      this.valueType,
      o.valueType) && java.util.Objects.equals(
      this.valueContainsNull,
      o.valueContainsNull);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(keyType) + 3 * java.util.Objects.hashCode(valueType) + 5 * java.util.Objects.hashCode(valueContainsNull);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MapType other) {
    int cmp = 0;
    cmp = ((Comparable) keyType).compareTo(other.keyType);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) valueType).compareTo(other.valueType);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) valueContainsNull).compareTo(other.valueContainsNull);
  }
  
  public MapType withKeyType(hydra.ext.io.delta.parquet.DataType keyType) {
    return new MapType(keyType, valueType, valueContainsNull);
  }
  
  public MapType withValueType(hydra.ext.io.delta.parquet.DataType valueType) {
    return new MapType(keyType, valueType, valueContainsNull);
  }
  
  public MapType withValueContainsNull(Boolean valueContainsNull) {
    return new MapType(keyType, valueType, valueContainsNull);
  }
}
