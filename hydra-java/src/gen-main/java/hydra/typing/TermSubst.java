// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * A substitution of term variables for terms
 */
public class TermSubst implements Serializable, Comparable<TermSubst> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.typing.TermSubst");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.Map<hydra.core.Name, hydra.core.Term> value;
  
  public TermSubst (java.util.Map<hydra.core.Name, hydra.core.Term> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermSubst)) {
      return false;
    }
    TermSubst o = (TermSubst) (other);
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
  public int compareTo(TermSubst other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
