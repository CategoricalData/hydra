package hydra.ext.java.syntax;

public class WhileStatement {
  public final java.util.Optional<Expression> cond;
  
  public final Statement body;
  
  public WhileStatement (java.util.Optional<Expression> cond, Statement body) {
    this.cond = cond;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhileStatement)) {
      return false;
    }
    WhileStatement o = (WhileStatement) (other);
    return cond.equals(o.cond) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * body.hashCode();
  }
  
  public WhileStatement withCond(java.util.Optional<Expression> cond) {
    return new WhileStatement(cond, body);
  }
  
  public WhileStatement withBody(Statement body) {
    return new WhileStatement(cond, body);
  }
}