// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association with a term-level field with a phantom type
 */
public class TField<A> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.phantoms.TField");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.core.Field value;
  
  public TField (hydra.core.Field value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TField)) {
      return false;
    }
    TField o = (TField) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}