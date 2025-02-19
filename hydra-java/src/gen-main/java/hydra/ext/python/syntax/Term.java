// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Term implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Term");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.TermLhs> lhs;
  
  public final hydra.ext.python.syntax.Factor rhs;
  
  public Term (hydra.util.Opt<hydra.ext.python.syntax.TermLhs> lhs, hydra.ext.python.syntax.Factor rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Term)) {
      return false;
    }
    Term o = (Term) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Term withLhs(hydra.util.Opt<hydra.ext.python.syntax.TermLhs> lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Term(lhs, rhs);
  }
  
  public Term withRhs(hydra.ext.python.syntax.Factor rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Term(lhs, rhs);
  }
}