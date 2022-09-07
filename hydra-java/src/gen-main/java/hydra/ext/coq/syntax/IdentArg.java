package hydra.ext.coq.syntax;

public class IdentArg {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.IdentArg");
  
  public final hydra.ext.coq.syntax.Ident ident;
  
  public final hydra.ext.coq.syntax.Term term;
  
  public IdentArg (hydra.ext.coq.syntax.Ident ident, hydra.ext.coq.syntax.Term term) {
    this.ident = ident;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IdentArg)) {
      return false;
    }
    IdentArg o = (IdentArg) (other);
    return ident.equals(o.ident) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * ident.hashCode() + 3 * term.hashCode();
  }
  
  public IdentArg withIdent(hydra.ext.coq.syntax.Ident ident) {
    return new IdentArg(ident, term);
  }
  
  public IdentArg withTerm(hydra.ext.coq.syntax.Term term) {
    return new IdentArg(ident, term);
  }
}