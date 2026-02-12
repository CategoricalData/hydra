// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class BasicForStatement implements Serializable, Comparable<BasicForStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.BasicForStatement");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
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
    BasicForStatement o = (BasicForStatement) other;
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
  public int compareTo(BasicForStatement other) {
    int cmp = 0;
    cmp = ((Comparable) cond).compareTo(other.cond);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public BasicForStatement withCond(hydra.ext.java.syntax.ForCond cond) {
    return new BasicForStatement(cond, body);
  }
  
  public BasicForStatement withBody(hydra.ext.java.syntax.Statement body) {
    return new BasicForStatement(cond, body);
  }
}
