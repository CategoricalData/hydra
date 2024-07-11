// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class IfThenElseStatement implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.IfThenElseStatement");
  
  public final hydra.util.Opt<hydra.langs.java.syntax.Expression> cond;
  
  public final hydra.langs.java.syntax.StatementNoShortIf then;
  
  public final hydra.langs.java.syntax.Statement else_;
  
  public IfThenElseStatement (hydra.util.Opt<hydra.langs.java.syntax.Expression> cond, hydra.langs.java.syntax.StatementNoShortIf then, hydra.langs.java.syntax.Statement else_) {
    if (cond == null) {
      throw new IllegalArgumentException("null value for 'cond' argument");
    }
    if (then == null) {
      throw new IllegalArgumentException("null value for 'then' argument");
    }
    if (else_ == null) {
      throw new IllegalArgumentException("null value for 'else' argument");
    }
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
    if (cond == null) {
      throw new IllegalArgumentException("null value for 'cond' argument");
    }
    return new IfThenElseStatement(cond, then, else_);
  }
  
  public IfThenElseStatement withThen(hydra.langs.java.syntax.StatementNoShortIf then) {
    if (then == null) {
      throw new IllegalArgumentException("null value for 'then' argument");
    }
    return new IfThenElseStatement(cond, then, else_);
  }
  
  public IfThenElseStatement withElse(hydra.langs.java.syntax.Statement else_) {
    if (else_ == null) {
      throw new IllegalArgumentException("null value for 'else' argument");
    }
    return new IfThenElseStatement(cond, then, else_);
  }
}