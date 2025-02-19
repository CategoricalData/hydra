// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

/**
 * An RFC3339-formatted timestamp in UTC (ending in 'Z')
 */
public class Timestamp implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.dev.osv.schema.Timestamp");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public Timestamp (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Timestamp)) {
      return false;
    }
    Timestamp o = (Timestamp) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}