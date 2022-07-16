package hydra.ext.java.syntax;

public class EnhancedForStatement {
  public final EnhancedForCond cond;
  
  public final Statement body;
  
  public EnhancedForStatement (EnhancedForCond cond, Statement body) {
    this.cond = cond;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnhancedForStatement)) {
      return false;
    }
    EnhancedForStatement o = (EnhancedForStatement) (other);
    return cond.equals(o.cond) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * body.hashCode();
  }
  
  public EnhancedForStatement withCond(EnhancedForCond cond) {
    return new EnhancedForStatement(cond, body);
  }
  
  public EnhancedForStatement withBody(Statement body) {
    return new EnhancedForStatement(cond, body);
  }
}