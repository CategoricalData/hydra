package hydra.ext.coq.syntax;

/**
 * Some constructions allow the binding of a variable to value. This is called a “let-binder”.
 */
public class LetBinder {
  public final Name name;
  
  public final java.util.Optional<Type> type;
  
  public final Term term;
  
  public LetBinder (Name name, java.util.Optional<Type> type, Term term) {
    this.name = name;
    this.type = type;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetBinder)) {
      return false;
    }
    LetBinder o = (LetBinder) (other);
    return name.equals(o.name) && type.equals(o.type) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode() + 5 * term.hashCode();
  }
  
  public LetBinder withName(Name name) {
    return new LetBinder(name, type, term);
  }
  
  public LetBinder withType(java.util.Optional<Type> type) {
    return new LetBinder(name, type, term);
  }
  
  public LetBinder withTerm(Term term) {
    return new LetBinder(name, type, term);
  }
}