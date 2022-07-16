package hydra.core;

/**
 * A type together with an instance of the type
 */
public class TypedTerm<M> {
  public final Type<M> type;
  
  public final Term<M> term;
  
  public TypedTerm (Type<M> type, Term<M> term) {
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
  
  public TypedTerm withType(Type<M> type) {
    return new TypedTerm(type, term);
  }
  
  public TypedTerm withTerm(Term<M> term) {
    return new TypedTerm(type, term);
  }
}