package hydra.core;

/**
 * A type together with an instance of the type
 */
public class TypedTerm<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.TypedTerm");
  
  public final hydra.core.Type<M> type;
  
  public final hydra.core.Term<M> term;
  
  public TypedTerm (hydra.core.Type<M> type, hydra.core.Term<M> term) {
    this.type = type;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedTerm)) {
      return false;
    }
    TypedTerm o = (TypedTerm) (other);
    return type.equals(o.type) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * term.hashCode();
  }
  
  public TypedTerm withType(hydra.core.Type<M> type) {
    return new TypedTerm(type, term);
  }
  
  public TypedTerm withTerm(hydra.core.Term<M> term) {
    return new TypedTerm(type, term);
  }
}