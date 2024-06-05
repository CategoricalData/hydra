package hydra.langs.parquet.delta;

import java.io.Serializable;

public class MapType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/delta.MapType");
  
  public final hydra.langs.parquet.delta.DataType keyType;
  
  public final hydra.langs.parquet.delta.DataType valueType;
  
  public final Boolean valueContainsNull;
  
  public MapType (hydra.langs.parquet.delta.DataType keyType, hydra.langs.parquet.delta.DataType valueType, Boolean valueContainsNull) {
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
    return new MapType(keyType, valueType, valueContainsNull);
  }
  
  public MapType withValueType(hydra.langs.parquet.delta.DataType valueType) {
    return new MapType(keyType, valueType, valueContainsNull);
  }
  
  public MapType withValueContainsNull(Boolean valueContainsNull) {
    return new MapType(keyType, valueType, valueContainsNull);
  }
}