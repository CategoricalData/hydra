package hydra.ext.coq.syntax;

public class IdentArg {
  public final Ident ident;
  
  public final Term term;
  
  public IdentArg (Ident ident, Term term) {
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
  
  public IdentArg withIdent(Ident ident) {
    return new IdentArg(ident, term);
  }
  
  public IdentArg withTerm(Term term) {
    return new IdentArg(ident, term);
  }
}