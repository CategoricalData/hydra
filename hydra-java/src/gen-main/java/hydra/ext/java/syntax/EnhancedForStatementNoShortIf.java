package hydra.ext.java.syntax;

public class EnhancedForStatementNoShortIf {
  public final EnhancedForCond cond;
  
  public final StatementNoShortIf body;
  
  public EnhancedForStatementNoShortIf (EnhancedForCond cond, StatementNoShortIf body) {
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
  
  public EnhancedForStatementNoShortIf withCond(EnhancedForCond cond) {
    return new EnhancedForStatementNoShortIf(cond, body);
  }
  
  public EnhancedForStatementNoShortIf withBody(StatementNoShortIf body) {
    return new EnhancedForStatementNoShortIf(cond, body);
  }
}