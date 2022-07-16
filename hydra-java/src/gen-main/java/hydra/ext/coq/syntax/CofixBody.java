package hydra.ext.coq.syntax;

public class CofixBody {
  public final Ident ident;
  
  public final java.util.List<Binder> binders;
  
  public final java.util.Optional<Type> type;
  
  public final Term term;
  
  public CofixBody (Ident ident, java.util.List<Binder> binders, java.util.Optional<Type> type, Term term) {
    this.ident = ident;
    this.binders = binders;
    this.type = type;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CofixBody)) {
      return false;
    }
    CofixBody o = (CofixBody) (other);
    return ident.equals(o.ident) && binders.equals(o.binders) && type.equals(o.type) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * ident.hashCode() + 3 * binders.hashCode() + 5 * type.hashCode() + 7 * term.hashCode();
  }
  
  public CofixBody withIdent(Ident ident) {
    return new CofixBody(ident, binders, type, term);
  }
  
  public CofixBody withBinders(java.util.List<Binder> binders) {
    return new CofixBody(ident, binders, type, term);
  }
  
  public CofixBody withType(java.util.Optional<Type> type) {
    return new CofixBody(ident, binders, type, term);
  }
  
  public CofixBody withTerm(Term term) {
    return new CofixBody(ident, binders, type, term);
  }
}