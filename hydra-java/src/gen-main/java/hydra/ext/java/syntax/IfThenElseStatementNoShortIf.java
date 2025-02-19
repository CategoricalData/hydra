// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class IfThenElseStatementNoShortIf implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.IfThenElseStatementNoShortIf");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_THEN = new hydra.core.Name("then");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Expression> cond;
  
  public final hydra.ext.java.syntax.StatementNoShortIf then;
  
  public final hydra.ext.java.syntax.StatementNoShortIf else_;
  
  public IfThenElseStatementNoShortIf (hydra.util.Opt<hydra.ext.java.syntax.Expression> cond, hydra.ext.java.syntax.StatementNoShortIf then, hydra.ext.java.syntax.StatementNoShortIf else_) {
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((then));
    java.util.Objects.requireNonNull((else_));
    this.cond = cond;
    this.then = then;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfThenElseStatementNoShortIf)) {
      return false;
    }
    IfThenElseStatementNoShortIf o = (IfThenElseStatementNoShortIf) (other);
    return cond.equals(o.cond) && then.equals(o.then) && else_.equals(o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * then.hashCode() + 5 * else_.hashCode();
  }
  
  public IfThenElseStatementNoShortIf withCond(hydra.util.Opt<hydra.ext.java.syntax.Expression> cond) {
    java.util.Objects.requireNonNull((cond));
    return new IfThenElseStatementNoShortIf(cond, then, else_);
  }
  
  public IfThenElseStatementNoShortIf withThen(hydra.ext.java.syntax.StatementNoShortIf then) {
    java.util.Objects.requireNonNull((then));
    return new IfThenElseStatementNoShortIf(cond, then, else_);
  }
  
  public IfThenElseStatementNoShortIf withElse(hydra.ext.java.syntax.StatementNoShortIf else_) {
    java.util.Objects.requireNonNull((else_));
    return new IfThenElseStatementNoShortIf(cond, then, else_);
  }
}