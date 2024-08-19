// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.delta;

import java.io.Serializable;

/**
 * Struct type which contains one or more columns.
 */
public class StructType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/parquet/delta.StructType");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final java.util.List<hydra.langs.parquet.delta.StructField> fields;
  
  public StructType (java.util.List<hydra.langs.parquet.delta.StructField> fields) {
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