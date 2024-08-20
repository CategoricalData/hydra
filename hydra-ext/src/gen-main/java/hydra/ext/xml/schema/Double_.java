// Note: this is an automatically generated file. Do not edit.

package hydra.ext.xml.schema;

import java.io.Serializable;

public class Double_ implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/xml/schema.Double");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final Double value;
  
  public Double_ (Double value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Double_)) {
      return false;
    }
    Double_ o = (Double_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
