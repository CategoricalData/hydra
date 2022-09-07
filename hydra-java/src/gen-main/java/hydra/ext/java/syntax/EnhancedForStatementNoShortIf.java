package hydra.ext.java.syntax;

public class EnhancedForStatementNoShortIf {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.EnhancedForStatementNoShortIf");
  
  public final hydra.ext.java.syntax.EnhancedForCond cond;
  
  public final hydra.ext.java.syntax.StatementNoShortIf body;
  
  public EnhancedForStatementNoShortIf (hydra.ext.java.syntax.EnhancedForCond cond, hydra.ext.java.syntax.StatementNoShortIf body) {
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
    return new EnhancedForStatementNoShortIf(cond, body);
  }
  
  public EnhancedForStatementNoShortIf withBody(hydra.ext.java.syntax.StatementNoShortIf body) {
    return new EnhancedForStatementNoShortIf(cond, body);
  }
}