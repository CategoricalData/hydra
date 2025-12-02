// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

/**
 * An association of a named term (element) with a phantom type
 */
public class TBinding<A> {
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
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((term));
    this.name = name;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TBinding)) {
      return false;
    }
    TBinding o = (TBinding) (other);
    return name.equals(o.name) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * term.hashCode();
  }
  
  public TBinding withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new TBinding(name, term);
  }
  
  public TBinding withTerm(hydra.phantoms.TTerm<A> term) {
    java.util.Objects.requireNonNull((term));
    return new TBinding(name, term);
  }
}
