package hydra.core;

/**
 * A term annotated with a fixed, named type; an instance of a newtype
 */
public class NominalTerm<A> {
  public final hydra.core.Name typeName;
  
  public final hydra.core.Term<A> term;
  
  /**
   * Constructs an immutable NominalTerm object
   */
  public NominalTerm(hydra.core.Name typeName, hydra.core.Term<A> term) {
    this.typeName = typeName;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NominalTerm)) {
        return false;
    }
    NominalTerm o = (NominalTerm) other;
    return typeName.equals(o.typeName)
        && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode()
        + 3 * term.hashCode();
  }
  
  /**
   * Construct a new immutable NominalTerm object in which typeName is overridden
   */
  public NominalTerm withTypeName(hydra.core.Name typeName) {
    return new NominalTerm(typeName, term);
  }
  
  /**
   * Construct a new immutable NominalTerm object in which term is overridden
   */
  public NominalTerm withTerm(hydra.core.Term<A> term) {
    return new NominalTerm(typeName, term);
  }
}
