// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnhancedForStatementNoShortIf implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.EnhancedForStatementNoShortIf");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.java.syntax.EnhancedForCond cond;
  
  public final hydra.ext.java.syntax.StatementNoShortIf body;
  
  public EnhancedForStatementNoShortIf (hydra.ext.java.syntax.EnhancedForCond cond, hydra.ext.java.syntax.StatementNoShortIf body) {
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((body));
    this.cond = cond;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnhancedForStatementNoShortIf)) {
      return false;
    }
    EnhancedForStatementNoShortIf o = (EnhancedForStatementNoShortIf) (other);
    return cond.equals(o.cond) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * body.hashCode();
  }
  
  public EnhancedForStatementNoShortIf withCond(hydra.ext.java.syntax.EnhancedForCond cond) {
    java.util.Objects.requireNonNull((cond));
    return new EnhancedForStatementNoShortIf(cond, body);
  }
  
  public EnhancedForStatementNoShortIf withBody(hydra.ext.java.syntax.StatementNoShortIf body) {
    java.util.Objects.requireNonNull((body));
    return new EnhancedForStatementNoShortIf(cond, body);
  }
}
