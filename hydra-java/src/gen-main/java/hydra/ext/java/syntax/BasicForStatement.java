// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class BasicForStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.BasicForStatement");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.java.syntax.ForCond cond;
  
  public final hydra.ext.java.syntax.Statement body;
  
  public BasicForStatement (hydra.ext.java.syntax.ForCond cond, hydra.ext.java.syntax.Statement body) {
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((body));
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
    java.util.Objects.requireNonNull((cond));
    return new BasicForStatement(cond, body);
  }
  
  public BasicForStatement withBody(hydra.ext.java.syntax.Statement body) {
    java.util.Objects.requireNonNull((body));
    return new BasicForStatement(cond, body);
  }
}