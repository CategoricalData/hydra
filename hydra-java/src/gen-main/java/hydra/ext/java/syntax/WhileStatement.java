// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class WhileStatement implements Serializable, Comparable<WhileStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.WhileStatement");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond;
  
  public final hydra.ext.java.syntax.Statement body;
  
  public WhileStatement (hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond, hydra.ext.java.syntax.Statement body) {
    this.cond = cond;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhileStatement)) {
      return false;
    }
    WhileStatement o = (WhileStatement) other;
    return java.util.Objects.equals(
      this.cond,
      o.cond) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cond) + 3 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WhileStatement other) {
    int cmp = 0;
    cmp = Integer.compare(
      cond.hashCode(),
      other.cond.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public WhileStatement withCond(hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond) {
    return new WhileStatement(cond, body);
  }
  
  public WhileStatement withBody(hydra.ext.java.syntax.Statement body) {
    return new WhileStatement(cond, body);
  }
}
