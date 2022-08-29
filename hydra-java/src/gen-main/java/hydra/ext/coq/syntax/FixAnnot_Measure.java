package hydra.ext.coq.syntax;

public class FixAnnot_Measure {
  public final hydra.ext.coq.syntax.OneTerm term;
  
  public final java.util.Optional<hydra.ext.coq.syntax.Ident> ident;
  
  public final java.util.Optional<hydra.ext.coq.syntax.OneTerm> term2;
  
  public FixAnnot_Measure (hydra.ext.coq.syntax.OneTerm term, java.util.Optional<hydra.ext.coq.syntax.Ident> ident, java.util.Optional<hydra.ext.coq.syntax.OneTerm> term2) {
    this.term = term;
    this.ident = ident;
    this.term2 = term2;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixAnnot_Measure)) {
      return false;
    }
    FixAnnot_Measure o = (FixAnnot_Measure) (other);
    return term.equals(o.term) && ident.equals(o.ident) && term2.equals(o.term2);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * ident.hashCode() + 5 * term2.hashCode();
  }
  
  public FixAnnot_Measure withTerm(hydra.ext.coq.syntax.OneTerm term) {
    return new FixAnnot_Measure(term, ident, term2);
  }
  
  public FixAnnot_Measure withIdent(java.util.Optional<hydra.ext.coq.syntax.Ident> ident) {
    return new FixAnnot_Measure(term, ident, term2);
  }
  
  public FixAnnot_Measure withTerm2(java.util.Optional<hydra.ext.coq.syntax.OneTerm> term2) {
    return new FixAnnot_Measure(term, ident, term2);
  }
}