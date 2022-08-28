package hydra.ext.java.syntax;

public class IfThenElseStatement {
  public final java.util.Optional<hydra.ext.java.syntax.Expression> cond;
  
  public final hydra.ext.java.syntax.StatementNoShortIf then;
  
  public final hydra.ext.java.syntax.Statement else_;
  
  public IfThenElseStatement (java.util.Optional<hydra.ext.java.syntax.Expression> cond, hydra.ext.java.syntax.StatementNoShortIf then, hydra.ext.java.syntax.Statement else_) {
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
  
  public IfThenElseStatement withCond(java.util.Optional<hydra.ext.java.syntax.Expression> cond) {
    return new IfThenElseStatement(cond, then, else_);
  }
  
  public IfThenElseStatement withThen(hydra.ext.java.syntax.StatementNoShortIf then) {
    return new IfThenElseStatement(cond, then, else_);
  }
  
  public IfThenElseStatement withElse(hydra.ext.java.syntax.Statement else_) {
    return new IfThenElseStatement(cond, then, else_);
  }
}