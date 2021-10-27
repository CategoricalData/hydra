package hydra.core;

public class TypedTerm {
  /**
   * @type hydra/core.Type
   */
  public final Type type;
  
  /**
   * @type hydra/core.Term
   */
  public final Term term;
  
  /**
   * Constructs an immutable TypedTerm object
   */
  public TypedTerm(Type type, Term term) {
    this.type = type;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedTerm)) {
        return false;
    }
    TypedTerm o = (TypedTerm) other;
    return type.equals(o.type)
        && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode()
        + 3 * term.hashCode();
  }
  
  /**
   * Construct a new immutable TypedTerm object in which type is overridden
   */
  public TypedTerm withType(Type type) {
    return new TypedTerm(type, term);
  }
  
  /**
   * Construct a new immutable TypedTerm object in which term is overridden
   */
  public TypedTerm withTerm(Term term) {
    return new TypedTerm(type, term);
  }
}
