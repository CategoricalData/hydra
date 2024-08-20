// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnhancedForStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.EnhancedForStatement");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.java.syntax.EnhancedForCond cond;
  
  public final hydra.ext.java.syntax.Statement body;
  
  public EnhancedForStatement (hydra.ext.java.syntax.EnhancedForCond cond, hydra.ext.java.syntax.Statement body) {
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((body));
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
    java.util.Objects.requireNonNull((cond));
    return new EnhancedForStatement(cond, body);
  }
  
  public EnhancedForStatement withBody(hydra.ext.java.syntax.Statement body) {
    java.util.Objects.requireNonNull((body));
    return new EnhancedForStatement(cond, body);
  }
}
