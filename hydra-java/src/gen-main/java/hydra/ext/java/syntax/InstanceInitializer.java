// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class InstanceInitializer implements Serializable, Comparable<InstanceInitializer> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.InstanceInitializer");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.java.syntax.Block value;
  
  public InstanceInitializer (hydra.ext.java.syntax.Block value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InstanceInitializer)) {
      return false;
    }
    InstanceInitializer o = (InstanceInitializer) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InstanceInitializer other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
