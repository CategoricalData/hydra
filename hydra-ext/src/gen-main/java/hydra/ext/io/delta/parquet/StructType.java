// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.delta.parquet;

import java.io.Serializable;

/**
 * Struct type which contains one or more columns.
 */
public class StructType implements Serializable, Comparable<StructType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.delta.parquet.StructType");
  
  public static final hydra.core.Name FIELDS = new hydra.core.Name("fields");
  
  public final java.util.List<hydra.ext.io.delta.parquet.StructField> fields;
  
  public StructType (java.util.List<hydra.ext.io.delta.parquet.StructField> fields) {
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StructType)) {
      return false;
    }
    StructType o = (StructType) other;
    return java.util.Objects.equals(
      this.fields,
      o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(fields);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StructType other) {
    return Integer.compare(
      fields.hashCode(),
      other.fields.hashCode());
  }
}
