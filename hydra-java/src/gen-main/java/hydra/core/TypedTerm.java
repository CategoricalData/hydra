package hydra.core;

public class TypedTerm {
  public final hydra.core.Type type;
  
  public final hydra.core.Term term;
  
  /**
   * Constructs an immutable TypedTerm object
   */
  public TypedTerm(hydra.core.Type type, hydra.core.Term term) {
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
  public TypedTerm withType(hydra.core.Type type) {
    return new TypedTerm(type, term);
  }
  
  /**
   * Construct a new immutable TypedTerm object in which term is overridden
   */
  public TypedTerm withTerm(hydra.core.Term term) {
    return new TypedTerm(type, term);
  }
}
