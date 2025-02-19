// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class TermList_Multiple implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.datalog.syntax.TermList_Multiple");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_TERM_LIST = new hydra.core.Name("termList");
  
  public final hydra.ext.datalog.syntax.Term term;
  
  public final hydra.ext.datalog.syntax.TermList termList;
  
  public TermList_Multiple (hydra.ext.datalog.syntax.Term term, hydra.ext.datalog.syntax.TermList termList) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((termList));
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
    java.util.Objects.requireNonNull((term));
    return new TermList_Multiple(term, termList);
  }
  
  public TermList_Multiple withTermList(hydra.ext.datalog.syntax.TermList termList) {
    java.util.Objects.requireNonNull((termList));
    return new TermList_Multiple(term, termList);
  }
}