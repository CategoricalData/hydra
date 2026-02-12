// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class IfThenElseStatementNoShortIf implements Serializable, Comparable<IfThenElseStatementNoShortIf> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.IfThenElseStatementNoShortIf");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_THEN = new hydra.core.Name("then");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond;
  
  public final hydra.ext.java.syntax.StatementNoShortIf then;
  
  public final hydra.ext.java.syntax.StatementNoShortIf else_;
  
  public IfThenElseStatementNoShortIf (hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond, hydra.ext.java.syntax.StatementNoShortIf then, hydra.ext.java.syntax.StatementNoShortIf else_) {
    this.cond = cond;
    this.then = then;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfThenElseStatementNoShortIf)) {
      return false;
    }
    IfThenElseStatementNoShortIf o = (IfThenElseStatementNoShortIf) other;
    return java.util.Objects.equals(
      this.cond,
      o.cond) && java.util.Objects.equals(
      this.then,
      o.then) && java.util.Objects.equals(
      this.else_,
      o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cond) + 3 * java.util.Objects.hashCode(then) + 5 * java.util.Objects.hashCode(else_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IfThenElseStatementNoShortIf other) {
    int cmp = 0;
    cmp = Integer.compare(
      cond.hashCode(),
      other.cond.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) then).compareTo(other.then);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) else_).compareTo(other.else_);
  }
  
  public IfThenElseStatementNoShortIf withCond(hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond) {
    return new IfThenElseStatementNoShortIf(cond, then, else_);
  }
  
  public IfThenElseStatementNoShortIf withThen(hydra.ext.java.syntax.StatementNoShortIf then) {
    return new IfThenElseStatementNoShortIf(cond, then, else_);
  }
  
  public IfThenElseStatementNoShortIf withElse(hydra.ext.java.syntax.StatementNoShortIf else_) {
    return new IfThenElseStatementNoShortIf(cond, then, else_);
  }
}
