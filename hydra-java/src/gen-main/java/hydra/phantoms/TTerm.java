// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association of a term with a phantom type
 */
public class TTerm<A> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.phantoms.TTerm");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.core.Term value;
  
  public TTerm (hydra.core.Term value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TTerm)) {
      return false;
    }
    TTerm o = (TTerm) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}