// Note: this is an automatically generated file. Do not edit.

package hydra.ext.xml.schema;

import java.io.Serializable;

public class Byte_ implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/xml/schema.Byte");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final Byte value;
  
  public Byte_ (Byte value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Byte_)) {
      return false;
    }
    Byte_ o = (Byte_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}