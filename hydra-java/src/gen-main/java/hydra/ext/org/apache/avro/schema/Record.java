// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public class Record implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.avro.schema.Record");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  /**
   * a JSON array, listing fields
   */
  public final java.util.List<hydra.ext.org.apache.avro.schema.Field> fields;
  
  public Record (java.util.List<hydra.ext.org.apache.avro.schema.Field> fields) {
    java.util.Objects.requireNonNull((fields));
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Record)) {
      return false;
    }
    Record o = (Record) (other);
    return fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * fields.hashCode();
  }
}