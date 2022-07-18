package hydra.ext.datalog.syntax;

public class TermList_Multiple {
  public final Term term;
  
  public final TermList termList;
  
  public TermList_Multiple (Term term, TermList termList) {
    this.term = term;
    this.termList = termList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermList_Multiple)) {
      return false;
    }
    TermList_Multiple o = (TermList_Multiple) (other);
    return term.equals(o.term) && termList.equals(o.termList);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * termList.hashCode();
  }
  
  public TermList_Multiple withTerm(Term term) {
    return new TermList_Multiple(term, termList);
  }
  
  public TermList_Multiple withTermList(TermList termList) {
    return new TermList_Multiple(term, termList);
  }
}