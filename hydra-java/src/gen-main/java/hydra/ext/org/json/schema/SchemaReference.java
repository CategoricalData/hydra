// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public class SchemaReference implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.SchemaReference");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public SchemaReference (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SchemaReference)) {
      return false;
    }
    SchemaReference o = (SchemaReference) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}