package hydra.ext.coq.syntax;

public class CofixBody {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.CofixBody");
  
  public final hydra.ext.coq.syntax.Ident ident;
  
  public final java.util.List<hydra.ext.coq.syntax.Binder> binders;
  
  public final java.util.Optional<hydra.ext.coq.syntax.Type> type;
  
  public final hydra.ext.coq.syntax.Term term;
  
  public CofixBody (hydra.ext.coq.syntax.Ident ident, java.util.List<hydra.ext.coq.syntax.Binder> binders, java.util.Optional<hydra.ext.coq.syntax.Type> type, hydra.ext.coq.syntax.Term term) {
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
  
  public CofixBody withIdent(hydra.ext.coq.syntax.Ident ident) {
    return new CofixBody(ident, binders, type, term);
  }
  
  public CofixBody withBinders(java.util.List<hydra.ext.coq.syntax.Binder> binders) {
    return new CofixBody(ident, binders, type, term);
  }
  
  public CofixBody withType(java.util.Optional<hydra.ext.coq.syntax.Type> type) {
    return new CofixBody(ident, binders, type, term);
  }
  
  public CofixBody withTerm(hydra.ext.coq.syntax.Term term) {
    return new CofixBody(ident, binders, type, term);
  }
}