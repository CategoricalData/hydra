// Note: this is an automatically generated file. Do not edit.

package hydra.langs.avro.schema;

import java.io.Serializable;

public class Array implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/avro/schema.Array");
  
  public final hydra.langs.avro.schema.Schema items;
  
  public Array (hydra.langs.avro.schema.Schema items) {
    if (items == null) {
      throw new IllegalArgumentException("null value for 'items' argument");
    }
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