// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association with a term-level field with a phantom type
 */
public class Fld<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/phantoms.Fld");
  
  public final hydra.core.Field value;
  
  public Fld (hydra.core.Field value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Fld)) {
      return false;
    }
    Fld o = (Fld) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}