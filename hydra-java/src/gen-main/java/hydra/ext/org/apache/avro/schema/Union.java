// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public class Union implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/avro/schema.Union");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.org.apache.avro.schema.Schema> value;
  
  public Union (java.util.List<hydra.ext.org.apache.avro.schema.Schema> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Union)) {
      return false;
    }
    Union o = (Union) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}