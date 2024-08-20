// Note: this is an automatically generated file. Do not edit.

package hydra.ext.xml.schema;

import java.io.Serializable;

public class ID implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/xml/schema.ID");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public ID (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ID)) {
      return false;
    }
    ID o = (ID) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}