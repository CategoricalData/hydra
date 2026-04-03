// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association of a term definition with a phantom type
 */
public class TTermDefinition<A> implements Serializable, Comparable<TTermDefinition<A>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.phantoms.TTermDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  /**
   * The name of the term
   */
  public final hydra.core.Name name;

  /**
   * The term with its phantom type
   */
  public final hydra.phantoms.TTerm<A> term;

  public TTermDefinition (hydra.core.Name name, hydra.phantoms.TTerm<A> term) {
    this.name = name;
    this.term = term;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TTermDefinition)) {
      return false;
    }
    TTermDefinition o = (TTermDefinition) other;
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
  public int compareTo(TTermDefinition other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) term).compareTo(other.term);
  }

  public TTermDefinition withName(hydra.core.Name name) {
    return new TTermDefinition(name, term);
  }

  public TTermDefinition withTerm(hydra.phantoms.TTerm<A> term) {
    return new TTermDefinition(name, term);
  }
}
