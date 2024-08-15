// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association of a field name (as in a case statement) with a phantom type
 */
public class TCase<A> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/phantoms.TCase");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.core.Name value;
  
  public TCase (hydra.core.Name value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TCase)) {
      return false;
    }
    TCase o = (TCase) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}