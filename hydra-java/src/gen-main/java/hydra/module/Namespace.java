// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A prefix for element names
 */
public class Namespace implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/module.Namespace");
  
  public final String value;
  
  public Namespace (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Namespace)) {
      return false;
    }
    Namespace o = (Namespace) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}