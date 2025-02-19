// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association with a named term (element) with a phantom type
 */
public class TElement<A> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.phantoms.TElement");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final hydra.core.Name name;
  
  public final hydra.phantoms.TTerm<A> term;
  
  public TElement (hydra.core.Name name, hydra.phantoms.TTerm<A> term) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((term));
    this.name = name;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TElement)) {
      return false;
    }
    TElement o = (TElement) (other);
    return name.equals(o.name) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * term.hashCode();
  }
  
  public TElement withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new TElement(name, term);
  }
  
  public TElement withTerm(hydra.phantoms.TTerm<A> term) {
    java.util.Objects.requireNonNull((term));
    return new TElement(name, term);
  }
}