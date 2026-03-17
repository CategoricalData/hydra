// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class IfThenElseStatement implements Serializable, Comparable<IfThenElseStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.IfThenElseStatement");

  public static final hydra.core.Name COND = new hydra.core.Name("cond");

  public static final hydra.core.Name THEN = new hydra.core.Name("then");

  public static final hydra.core.Name ELSE = new hydra.core.Name("else");

  public final hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond;

  public final hydra.ext.java.syntax.StatementNoShortIf then;

  public final hydra.ext.java.syntax.Statement else_;

  public IfThenElseStatement (hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond, hydra.ext.java.syntax.StatementNoShortIf then, hydra.ext.java.syntax.Statement else_) {
    this.cond = cond;
    this.then = then;
    this.else_ = else_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfThenElseStatement)) {
      return false;
    }
    IfThenElseStatement o = (IfThenElseStatement) other;
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
  public int compareTo(IfThenElseStatement other) {
    int cmp = 0;
    cmp = ((Comparable) cond).compareTo(other.cond);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) then).compareTo(other.then);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) else_).compareTo(other.else_);
  }

  public IfThenElseStatement withCond(hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond) {
    return new IfThenElseStatement(cond, then, else_);
  }

  public IfThenElseStatement withThen(hydra.ext.java.syntax.StatementNoShortIf then) {
    return new IfThenElseStatement(cond, then, else_);
  }

  public IfThenElseStatement withElse(hydra.ext.java.syntax.Statement else_) {
    return new IfThenElseStatement(cond, then, else_);
  }
}
