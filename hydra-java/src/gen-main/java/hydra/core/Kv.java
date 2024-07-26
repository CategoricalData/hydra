// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A key/value map which serves as a built-in metadata container for terms
 */
public class Kv implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Kv");
  
  /**
   * A map of annotation names to annotation values
   */
  public final java.util.Map<String, hydra.core.Term> annotations;
  
  public Kv (java.util.Map<String, hydra.core.Term> annotations) {
    java.util.Objects.requireNonNull((annotations));
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Kv)) {
      return false;
    }
    Kv o = (Kv) (other);
    return annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode();
  }
}