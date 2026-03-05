// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class TermList_Multiple implements Serializable, Comparable<TermList_Multiple> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.datalog.syntax.TermList_Multiple");
  
  public static final hydra.core.Name TERM = new hydra.core.Name("Term");
  
  public static final hydra.core.Name TERM_LIST = new hydra.core.Name("TermList");
  
  public final hydra.ext.datalog.syntax.Term Term;
  
  public final hydra.ext.datalog.syntax.TermList TermList;
  
  public TermList_Multiple (hydra.ext.datalog.syntax.Term Term, hydra.ext.datalog.syntax.TermList TermList) {
    this.Term = Term;
    this.TermList = TermList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermList_Multiple)) {
      return false;
    }
    TermList_Multiple o = (TermList_Multiple) other;
    return java.util.Objects.equals(
      this.Term,
      o.Term) && java.util.Objects.equals(
      this.TermList,
      o.TermList);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Term) + 3 * java.util.Objects.hashCode(TermList);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TermList_Multiple other) {
    int cmp = 0;
    cmp = ((Comparable) Term).compareTo(other.Term);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) TermList).compareTo(other.TermList);
  }
  
  public TermList_Multiple withTerm(hydra.ext.datalog.syntax.Term Term) {
    return new TermList_Multiple(Term, TermList);
  }
  
  public TermList_Multiple withTermList(hydra.ext.datalog.syntax.TermList TermList) {
    return new TermList_Multiple(Term, TermList);
  }
}
