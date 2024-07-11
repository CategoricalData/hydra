// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.delta;

import java.io.Serializable;

public class StructType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/delta.StructType");
  
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