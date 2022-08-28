package hydra.ext.coq.syntax;

public class FixAnnot_Wf {
  public final hydra.ext.coq.syntax.OneTerm term;
  
  public final hydra.ext.coq.syntax.Ident ident;
  
  public FixAnnot_Wf (hydra.ext.coq.syntax.OneTerm term, hydra.ext.coq.syntax.Ident ident) {
    this.term = term;
    this.ident = ident;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixAnnot_Wf)) {
      return false;
    }
    FixAnnot_Wf o = (FixAnnot_Wf) (other);
    return term.equals(o.term) && ident.equals(o.ident);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * ident.hashCode();
  }
  
  public FixAnnot_Wf withTerm(hydra.ext.coq.syntax.OneTerm term) {
    return new FixAnnot_Wf(term, ident);
  }
  
  public FixAnnot_Wf withIdent(hydra.ext.coq.syntax.Ident ident) {
    return new FixAnnot_Wf(term, ident);
  }
}