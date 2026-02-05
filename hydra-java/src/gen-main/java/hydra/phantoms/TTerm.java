// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association of a term with a phantom type
 */
public class TTerm<A> implements Serializable, Comparable<TTerm<A>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.phantoms.TTerm");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.core.Term value;
  
  public TTerm (hydra.core.Term value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TTerm)) {
      return false;
    }
    TTerm o = (TTerm) (other);
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
  public int compareTo(TTerm other) {
    return ((Comparable) (value)).compareTo(other.value);
  }
}
