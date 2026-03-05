// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.delta.parquet;

import java.io.Serializable;

/**
 * Represents a subfield of StructType with additional properties and metadata.
 */
public class StructField implements Serializable, Comparable<StructField> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.delta.parquet.StructField");
  
  public static final hydra.core.Name NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name DATA_TYPE = new hydra.core.Name("dataType");
  
  public static final hydra.core.Name NULLABLE = new hydra.core.Name("nullable");
  
  public final String name;
  
  public final hydra.ext.io.delta.parquet.DataType dataType;
  
  public final Boolean nullable;
  
  public StructField (String name, hydra.ext.io.delta.parquet.DataType dataType, Boolean nullable) {
    this.name = name;
    this.dataType = dataType;
    this.nullable = nullable;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StructField)) {
      return false;
    }
    StructField o = (StructField) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.dataType,
      o.dataType) && java.util.Objects.equals(
      this.nullable,
      o.nullable);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(dataType) + 5 * java.util.Objects.hashCode(nullable);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StructField other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) dataType).compareTo(other.dataType);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) nullable).compareTo(other.nullable);
  }
  
  public StructField withName(String name) {
    return new StructField(name, dataType, nullable);
  }
  
  public StructField withDataType(hydra.ext.io.delta.parquet.DataType dataType) {
    return new StructField(name, dataType, nullable);
  }
  
  public StructField withNullable(Boolean nullable) {
    return new StructField(name, dataType, nullable);
  }
}
