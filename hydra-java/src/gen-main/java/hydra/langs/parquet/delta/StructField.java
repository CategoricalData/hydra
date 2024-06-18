package hydra.langs.parquet.delta;

import java.io.Serializable;

public class StructField implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/delta.StructField");
  
  public final String name;
  
  public final hydra.langs.parquet.delta.DataType dataType;
  
  public final Boolean nullable;
  
  public StructField (String name, hydra.langs.parquet.delta.DataType dataType, Boolean nullable) {
    this.name = name;
    this.dataType = dataType;
    this.nullable = nullable;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StructField)) {
      return false;
    }
    StructField o = (StructField) (other);
    return name.equals(o.name) && dataType.equals(o.dataType) && nullable.equals(o.nullable);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * dataType.hashCode() + 5 * nullable.hashCode();
  }
  
  public StructField withName(String name) {
    return new StructField(name, dataType, nullable);
  }
  
  public StructField withDataType(hydra.langs.parquet.delta.DataType dataType) {
    return new StructField(name, dataType, nullable);
  }
  
  public StructField withNullable(Boolean nullable) {
    return new StructField(name, dataType, nullable);
  }
}