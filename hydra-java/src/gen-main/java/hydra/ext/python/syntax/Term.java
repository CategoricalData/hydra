// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Term implements Serializable, Comparable<Term> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Term");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.TermLhs> lhs;
  
  public final hydra.ext.python.syntax.Factor rhs;
  
  public Term (hydra.util.Maybe<hydra.ext.python.syntax.TermLhs> lhs, hydra.ext.python.syntax.Factor rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Term)) {
      return false;
    }
    Term o = (Term) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(rhs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Term other) {
    int cmp = 0;
    cmp = Integer.compare(
      lhs.hashCode(),
      other.lhs.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public Term withLhs(hydra.util.Maybe<hydra.ext.python.syntax.TermLhs> lhs) {
    return new Term(lhs, rhs);
  }
  
  public Term withRhs(hydra.ext.python.syntax.Factor rhs) {
    return new Term(lhs, rhs);
  }
}
