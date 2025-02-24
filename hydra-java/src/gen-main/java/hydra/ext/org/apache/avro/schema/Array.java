// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public class Array implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.avro.schema.Array");
  
  public static final hydra.core.Name FIELD_NAME_ITEMS = new hydra.core.Name("items");
  
  public final hydra.ext.org.apache.avro.schema.Schema items;
  
  public Array (hydra.ext.org.apache.avro.schema.Schema items) {
    java.util.Objects.requireNonNull((items));
    this.items = items;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Array)) {
      return false;
    }
    Array o = (Array) (other);
    return items.equals(o.items);
  }
  
  @Override
  public int hashCode() {
    return 2 * items.hashCode();
  }
}