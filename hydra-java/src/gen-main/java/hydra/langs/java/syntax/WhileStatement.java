// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class WhileStatement implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.WhileStatement");
  
  public final hydra.util.Opt<hydra.langs.java.syntax.Expression> cond;
  
  public final hydra.langs.java.syntax.Statement body;
  
  public WhileStatement (hydra.util.Opt<hydra.langs.java.syntax.Expression> cond, hydra.langs.java.syntax.Statement body) {
    if (cond == null) {
      throw new IllegalArgumentException("null value for 'cond' argument");
    }
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
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
  
  public WhileStatement withCond(hydra.util.Opt<hydra.langs.java.syntax.Expression> cond) {
    if (cond == null) {
      throw new IllegalArgumentException("null value for 'cond' argument");
    }
    return new WhileStatement(cond, body);
  }
  
  public WhileStatement withBody(hydra.langs.java.syntax.Statement body) {
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    return new WhileStatement(cond, body);
  }
}