// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.delta;

import java.io.Serializable;

public class MapType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/delta.MapType");
  
  public final hydra.langs.parquet.delta.DataType keyType;
  
  public final hydra.langs.parquet.delta.DataType valueType;
  
  public final Boolean valueContainsNull;
  
  public MapType (hydra.langs.parquet.delta.DataType keyType, hydra.langs.parquet.delta.DataType valueType, Boolean valueContainsNull) {
    if (keyType == null) {
      throw new IllegalArgumentException("null value for 'keyType' argument");
    }
    if (valueType == null) {
      throw new IllegalArgumentException("null value for 'valueType' argument");
    }
    if (valueContainsNull == null) {
      throw new IllegalArgumentException("null value for 'valueContainsNull' argument");
    }
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
  
  public MapType withKeyType(hydra.langs.parquet.delta.DataType keyType) {
    if (keyType == null) {
      throw new IllegalArgumentException("null value for 'keyType' argument");
    }
    return new MapType(keyType, valueType, valueContainsNull);
  }
  
  public MapType withValueType(hydra.langs.parquet.delta.DataType valueType) {
    if (valueType == null) {
      throw new IllegalArgumentException("null value for 'valueType' argument");
    }
    return new MapType(keyType, valueType, valueContainsNull);
  }
  
  public MapType withValueContainsNull(Boolean valueContainsNull) {
    if (valueContainsNull == null) {
      throw new IllegalArgumentException("null value for 'valueContainsNull' argument");
    }
    return new MapType(keyType, valueType, valueContainsNull);
  }
}