package hydra.ext.coq.syntax;

/**
 * Some constructions allow the binding of a variable to value. This is called a “let-binder”.
 */
public class LetBinder {
  public final hydra.ext.coq.syntax.Name name;
  
  public final java.util.Optional<hydra.ext.coq.syntax.Type> type;
  
  public final hydra.ext.coq.syntax.Term term;
  
  public LetBinder (hydra.ext.coq.syntax.Name name, java.util.Optional<hydra.ext.coq.syntax.Type> type, hydra.ext.coq.syntax.Term term) {
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
  
  public LetBinder withName(hydra.ext.coq.syntax.Name name) {
    return new LetBinder(name, type, term);
  }
  
  public LetBinder withType(java.util.Optional<hydra.ext.coq.syntax.Type> type) {
    return new LetBinder(name, type, term);
  }
  
  public LetBinder withTerm(hydra.ext.coq.syntax.Term term) {
    return new LetBinder(name, type, term);
  }
}