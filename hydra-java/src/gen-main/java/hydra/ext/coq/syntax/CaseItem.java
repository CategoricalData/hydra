package hydra.ext.coq.syntax;

public class CaseItem {
  public final hydra.ext.coq.syntax.Term100 term;
  
  public final java.util.Optional<hydra.ext.coq.syntax.Name> as;
  
  public final java.util.Optional<hydra.ext.coq.syntax.Pattern> in;
  
  public CaseItem (hydra.ext.coq.syntax.Term100 term, java.util.Optional<hydra.ext.coq.syntax.Name> as, java.util.Optional<hydra.ext.coq.syntax.Pattern> in) {
    this.term = term;
    this.as = as;
    this.in = in;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseItem)) {
      return false;
    }
    CaseItem o = (CaseItem) (other);
    return term.equals(o.term) && as.equals(o.as) && in.equals(o.in);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * as.hashCode() + 5 * in.hashCode();
  }
  
  public CaseItem withTerm(hydra.ext.coq.syntax.Term100 term) {
    return new CaseItem(term, as, in);
  }
  
  public CaseItem withAs(java.util.Optional<hydra.ext.coq.syntax.Name> as) {
    return new CaseItem(term, as, in);
  }
  
  public CaseItem withIn(java.util.Optional<hydra.ext.coq.syntax.Pattern> in) {
    return new CaseItem(term, as, in);
  }
}