// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * A substitution of term variables for terms
 */
public class TermSubst implements Serializable, Comparable<TermSubst> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.typing.TermSubst");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> value;
  
  public TermSubst (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermSubst)) {
      return false;
    }
    TermSubst o = (TermSubst) other;
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
    return ((Comparable) value).compareTo(other.value);
  }
}
