package hydra.ext.java.syntax;

public class IfThenElseStatement {
  public final java.util.Optional<Expression> cond;
  
  public final StatementNoShortIf then;
  
  public final Statement else_;
  
  public IfThenElseStatement (java.util.Optional<Expression> cond, StatementNoShortIf then, Statement else_) {
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
  
  public IfThenElseStatement withCond(java.util.Optional<Expression> cond) {
    return new IfThenElseStatement(cond, then, else_);
  }
  
  public IfThenElseStatement withThen(StatementNoShortIf then) {
    return new IfThenElseStatement(cond, then, else_);
  }
  
  public IfThenElseStatement withElse(Statement else_) {
    return new IfThenElseStatement(cond, then, else_);
  }
}