package hydra.ext.coq.syntax;

public class CaseItem {
  public final Term100 term;
  
  public final java.util.Optional<Name> as;
  
  public final java.util.Optional<Pattern> in;
  
  public CaseItem (Term100 term, java.util.Optional<Name> as, java.util.Optional<Pattern> in) {
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
  
  public CaseItem withTerm(Term100 term) {
    return new CaseItem(term, as, in);
  }
  
  public CaseItem withAs(java.util.Optional<Name> as) {
    return new CaseItem(term, as, in);
  }
  
  public CaseItem withIn(java.util.Optional<Pattern> in) {
    return new CaseItem(term, as, in);
  }
}