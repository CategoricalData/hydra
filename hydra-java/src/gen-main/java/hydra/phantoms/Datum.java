// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association of a term with a phantom type
 */
public class Datum<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/phantoms.Datum");
  
  public final hydra.core.Term<hydra.compute.Kv> value;
  
  public Datum (hydra.core.Term<hydra.compute.Kv> value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Datum)) {
      return false;
    }
    Datum o = (Datum) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}