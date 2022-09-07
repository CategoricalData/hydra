package hydra.ext.java.syntax;

public class BasicForStatement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.BasicForStatement");
  
  public final hydra.ext.java.syntax.ForCond cond;
  
  public final hydra.ext.java.syntax.Statement body;
  
  public BasicForStatement (hydra.ext.java.syntax.ForCond cond, hydra.ext.java.syntax.Statement body) {
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
  
  public BasicForStatement withCond(hydra.ext.java.syntax.ForCond cond) {
    return new BasicForStatement(cond, body);
  }
  
  public BasicForStatement withBody(hydra.ext.java.syntax.Statement body) {
    return new BasicForStatement(cond, body);
  }
}