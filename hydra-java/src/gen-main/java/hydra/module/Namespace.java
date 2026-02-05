// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A prefix for element names
 */
public class Namespace implements Serializable, Comparable<Namespace> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.module.Namespace");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public Namespace (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Namespace)) {
      return false;
    }
    Namespace o = (Namespace) (other);
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
  public int compareTo(Namespace other) {
    return ((Comparable) (value)).compareTo(other.value);
  }
}
