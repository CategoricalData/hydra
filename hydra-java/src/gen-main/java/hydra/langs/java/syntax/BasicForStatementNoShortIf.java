// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class BasicForStatementNoShortIf implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.BasicForStatementNoShortIf");
  
  public final hydra.langs.java.syntax.ForCond cond;
  
  public final hydra.langs.java.syntax.StatementNoShortIf body;
  
  public BasicForStatementNoShortIf (hydra.langs.java.syntax.ForCond cond, hydra.langs.java.syntax.StatementNoShortIf body) {
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
    if (!(other instanceof BasicForStatementNoShortIf)) {
      return false;
    }
    BasicForStatementNoShortIf o = (BasicForStatementNoShortIf) (other);
    return cond.equals(o.cond) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * body.hashCode();
  }
  
  public BasicForStatementNoShortIf withCond(hydra.langs.java.syntax.ForCond cond) {
    if (cond == null) {
      throw new IllegalArgumentException("null value for 'cond' argument");
    }
    return new BasicForStatementNoShortIf(cond, body);
  }
  
  public BasicForStatementNoShortIf withBody(hydra.langs.java.syntax.StatementNoShortIf body) {
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    return new BasicForStatementNoShortIf(cond, body);
  }
}