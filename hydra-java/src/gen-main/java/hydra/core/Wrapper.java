package hydra.core;

/**
 * A term wrapped in a type name; an instance of a newtype
 */
public class Wrapper<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Wrapper");
  
  public final hydra.core.Name typeName;
  
  public final hydra.core.Term<M> term;
  
  public Wrapper (hydra.core.Name typeName, hydra.core.Term<M> term) {
    this.typeName = typeName;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Wrapper)) {
      return false;
    }
    Wrapper o = (Wrapper) (other);
    return typeName.equals(o.typeName) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * term.hashCode();
  }
  
  public Wrapper withTypeName(hydra.core.Name typeName) {
    return new Wrapper(typeName, term);
  }
  
  public Wrapper withTerm(hydra.core.Term<M> term) {
    return new Wrapper(typeName, term);
  }
}