package hydra.ext.datalog.syntax;

public class TermList_Multiple {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/datalog/syntax.TermList.Multiple");
  
  public final hydra.ext.datalog.syntax.Term term;
  
  public final hydra.ext.datalog.syntax.TermList termList;
  
  public TermList_Multiple (hydra.ext.datalog.syntax.Term term, hydra.ext.datalog.syntax.TermList termList) {
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
  
  public TermList_Multiple withTerm(hydra.ext.datalog.syntax.Term term) {
    return new TermList_Multiple(term, termList);
  }
  
  public TermList_Multiple withTermList(hydra.ext.datalog.syntax.TermList termList) {
    return new TermList_Multiple(term, termList);
  }
}