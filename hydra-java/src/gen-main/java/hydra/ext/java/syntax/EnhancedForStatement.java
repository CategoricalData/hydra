package hydra.ext.java.syntax;

public class EnhancedForStatement {
  public final hydra.ext.java.syntax.EnhancedForCond cond;
  
  public final hydra.ext.java.syntax.Statement body;
  
  public EnhancedForStatement (hydra.ext.java.syntax.EnhancedForCond cond, hydra.ext.java.syntax.Statement body) {
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
  
  public EnhancedForStatement withCond(hydra.ext.java.syntax.EnhancedForCond cond) {
    return new EnhancedForStatement(cond, body);
  }
  
  public EnhancedForStatement withBody(hydra.ext.java.syntax.Statement body) {
    return new EnhancedForStatement(cond, body);
  }
}