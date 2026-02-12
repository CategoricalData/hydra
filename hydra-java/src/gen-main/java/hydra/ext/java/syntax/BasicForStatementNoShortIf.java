// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class BasicForStatementNoShortIf implements Serializable, Comparable<BasicForStatementNoShortIf> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.BasicForStatementNoShortIf");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.java.syntax.ForCond cond;
  
  public final hydra.ext.java.syntax.StatementNoShortIf body;
  
  public BasicForStatementNoShortIf (hydra.ext.java.syntax.ForCond cond, hydra.ext.java.syntax.StatementNoShortIf body) {
    this.cond = cond;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BasicForStatementNoShortIf)) {
      return false;
    }
    BasicForStatementNoShortIf o = (BasicForStatementNoShortIf) other;
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
  public int compareTo(BasicForStatementNoShortIf other) {
    int cmp = 0;
    cmp = ((Comparable) cond).compareTo(other.cond);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public BasicForStatementNoShortIf withCond(hydra.ext.java.syntax.ForCond cond) {
    return new BasicForStatementNoShortIf(cond, body);
  }
  
  public BasicForStatementNoShortIf withBody(hydra.ext.java.syntax.StatementNoShortIf body) {
    return new BasicForStatementNoShortIf(cond, body);
  }
}
