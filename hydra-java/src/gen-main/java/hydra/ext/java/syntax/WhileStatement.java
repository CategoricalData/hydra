// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class WhileStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.WhileStatement");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Expression> cond;
  
  public final hydra.ext.java.syntax.Statement body;
  
  public WhileStatement (hydra.util.Opt<hydra.ext.java.syntax.Expression> cond, hydra.ext.java.syntax.Statement body) {
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((body));
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
  
  public WhileStatement withCond(hydra.util.Opt<hydra.ext.java.syntax.Expression> cond) {
    java.util.Objects.requireNonNull((cond));
    return new WhileStatement(cond, body);
  }
  
  public WhileStatement withBody(hydra.ext.java.syntax.Statement body) {
    java.util.Objects.requireNonNull((body));
    return new WhileStatement(cond, body);
  }
}
