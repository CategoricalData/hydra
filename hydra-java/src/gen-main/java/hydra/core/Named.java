package hydra.core;

/**
 * A term annotated with a fixed, named type; an instance of a newtype
 */
public class Named<M> {
  public final hydra.core.Name typeName;
  
  public final hydra.core.Term<M> term;
  
  public Named (hydra.core.Name typeName, hydra.core.Term<M> term) {
    this.typeName = typeName;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Named)) {
      return false;
    }
    Named o = (Named) (other);
    return typeName.equals(o.typeName) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * term.hashCode();
  }
  
  public Named withTypeName(hydra.core.Name typeName) {
    return new Named(typeName, term);
  }
  
  public Named withTerm(hydra.core.Term<M> term) {
    return new Named(typeName, term);
  }
}