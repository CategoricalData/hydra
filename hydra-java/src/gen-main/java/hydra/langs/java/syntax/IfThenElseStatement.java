// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class IfThenElseStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.IfThenElseStatement");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_THEN = new hydra.core.Name("then");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final hydra.util.Opt<hydra.langs.java.syntax.Expression> cond;
  
  public final hydra.langs.java.syntax.StatementNoShortIf then;
  
  public final hydra.langs.java.syntax.Statement else_;
  
  public IfThenElseStatement (hydra.util.Opt<hydra.langs.java.syntax.Expression> cond, hydra.langs.java.syntax.StatementNoShortIf then, hydra.langs.java.syntax.Statement else_) {
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((then));
    java.util.Objects.requireNonNull((else_));
    this.cond = cond;
    this.then = then;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfThenElseStatement)) {
      return false;
    }
    IfThenElseStatement o = (IfThenElseStatement) (other);
    return cond.equals(o.cond) && then.equals(o.then) && else_.equals(o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * then.hashCode() + 5 * else_.hashCode();
  }
  
  public IfThenElseStatement withCond(hydra.util.Opt<hydra.langs.java.syntax.Expression> cond) {
    java.util.Objects.requireNonNull((cond));
    return new IfThenElseStatement(cond, then, else_);
  }
  
  public IfThenElseStatement withThen(hydra.langs.java.syntax.StatementNoShortIf then) {
    java.util.Objects.requireNonNull((then));
    return new IfThenElseStatement(cond, then, else_);
  }
  
  public IfThenElseStatement withElse(hydra.langs.java.syntax.Statement else_) {
    java.util.Objects.requireNonNull((else_));
    return new IfThenElseStatement(cond, then, else_);
  }
}