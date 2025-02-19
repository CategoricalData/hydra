// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public class Map implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.avro.schema.Map");
  
  public static final hydra.core.Name FIELD_NAME_VALUES = new hydra.core.Name("values");
  
  public final hydra.ext.org.apache.avro.schema.Schema values;
  
  public Map (hydra.ext.org.apache.avro.schema.Schema values) {
    java.util.Objects.requireNonNull((values));
    this.values = values;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Map)) {
      return false;
    }
    Map o = (Map) (other);
    return values.equals(o.values);
  }
  
  @Override
  public int hashCode() {
    return 2 * values.hashCode();
  }
}