package hydra.ext.java.syntax;

public class WhileStatement {
  public final java.util.Optional<hydra.ext.java.syntax.Expression> cond;
  
  public final hydra.ext.java.syntax.Statement body;
  
  public WhileStatement (java.util.Optional<hydra.ext.java.syntax.Expression> cond, hydra.ext.java.syntax.Statement body) {
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
  
  public WhileStatement withCond(java.util.Optional<hydra.ext.java.syntax.Expression> cond) {
    return new WhileStatement(cond, body);
  }
  
  public WhileStatement withBody(hydra.ext.java.syntax.Statement body) {
    return new WhileStatement(cond, body);
  }
}