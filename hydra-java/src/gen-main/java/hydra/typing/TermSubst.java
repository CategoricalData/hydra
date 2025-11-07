// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * A substitution of term variables for terms
 */
public class TermSubst implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.typing.TermSubst");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.Map<hydra.core.Name, hydra.core.Term> value;
  
  public TermSubst (java.util.Map<hydra.core.Name, hydra.core.Term> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermSubst)) {
      return false;
    }
    TermSubst o = (TermSubst) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
