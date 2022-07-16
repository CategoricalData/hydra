package hydra.ext.java.syntax;

public class BasicForStatement {
  public final ForCond cond;
  
  public final Statement body;
  
  public BasicForStatement (ForCond cond, Statement body) {
    this.cond = cond;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BasicForStatement)) {
      return false;
    }
    BasicForStatement o = (BasicForStatement) (other);
    return cond.equals(o.cond) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * body.hashCode();
  }
  
  public BasicForStatement withCond(ForCond cond) {
    return new BasicForStatement(cond, body);
  }
  
  public BasicForStatement withBody(Statement body) {
    return new BasicForStatement(cond, body);
  }
}