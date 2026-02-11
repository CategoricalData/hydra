// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association of a named term (element) with a phantom type
 */
public class TBinding<A> implements Serializable, Comparable<TBinding<A>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.phantoms.TBinding");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  /**
   * The name of the term
   */
  public final hydra.core.Name name;
  
  /**
   * The term with its phantom type
   */
  public final hydra.phantoms.TTerm<A> term;
  
  public TBinding (hydra.core.Name name, hydra.phantoms.TTerm<A> term) {
    this.name = name;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TBinding)) {
      return false;
    }
    TBinding o = (TBinding) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.term,
      o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(term);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TBinding other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) term).compareTo(other.term);
  }
  
  public TBinding withName(hydra.core.Name name) {
    return new TBinding(name, term);
  }
  
  public TBinding withTerm(hydra.phantoms.TTerm<A> term) {
    return new TBinding(name, term);
  }
}
