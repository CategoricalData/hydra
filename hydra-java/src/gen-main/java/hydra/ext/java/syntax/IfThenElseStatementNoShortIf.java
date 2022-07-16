package hydra.ext.java.syntax;

public class IfThenElseStatementNoShortIf {
  public final java.util.Optional<Expression> cond;
  
  public final StatementNoShortIf then;
  
  public final StatementNoShortIf else_;
  
  public IfThenElseStatementNoShortIf (java.util.Optional<Expression> cond, StatementNoShortIf then, StatementNoShortIf else_) {
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
  
  public IfThenElseStatementNoShortIf withCond(java.util.Optional<Expression> cond) {
    return new IfThenElseStatementNoShortIf(cond, then, else_);
  }
  
  public IfThenElseStatementNoShortIf withThen(StatementNoShortIf then) {
    return new IfThenElseStatementNoShortIf(cond, then, else_);
  }
  
  public IfThenElseStatementNoShortIf withElse(StatementNoShortIf else_) {
    return new IfThenElseStatementNoShortIf(cond, then, else_);
  }
}