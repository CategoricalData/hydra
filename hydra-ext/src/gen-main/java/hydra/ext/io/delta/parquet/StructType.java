// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.delta.parquet;

import java.io.Serializable;

/**
 * Struct type which contains one or more columns.
 */
public class StructType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/delta/parquet.StructType");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final java.util.List<hydra.ext.io.delta.parquet.StructField> fields;
  
  public StructType (java.util.List<hydra.ext.io.delta.parquet.StructField> fields) {
    java.util.Objects.requireNonNull((fields));
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StructType)) {
      return false;
    }
    StructType o = (StructType) (other);
    return fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * fields.hashCode();
  }
}