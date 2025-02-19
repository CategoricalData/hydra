// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

/**
 * The value CVSS_V3, or future supported types
 */
public class SeverityType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.dev.osv.schema.SeverityType");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public SeverityType (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SeverityType)) {
      return false;
    }
    SeverityType o = (SeverityType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}