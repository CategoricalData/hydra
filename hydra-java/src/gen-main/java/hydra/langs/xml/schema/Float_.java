// Note: this is an automatically generated file. Do not edit.

package hydra.langs.xml.schema;

import java.io.Serializable;

public class Float_ implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/xml/schema.Float");
  
  public final Float value;
  
  public Float_ (Float value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Float_)) {
      return false;
    }
    Float_ o = (Float_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}