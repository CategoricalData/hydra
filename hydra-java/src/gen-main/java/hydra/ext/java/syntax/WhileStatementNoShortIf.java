package hydra.ext.java.syntax;

public class WhileStatementNoShortIf {
  public final java.util.Optional<Expression> cond;
  
  public final StatementNoShortIf body;
  
  public WhileStatementNoShortIf (java.util.Optional<Expression> cond, StatementNoShortIf body) {
    this.cond = cond;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhileStatementNoShortIf)) {
      return false;
    }
    WhileStatementNoShortIf o = (WhileStatementNoShortIf) (other);
    return cond.equals(o.cond) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * body.hashCode();
  }
  
  public WhileStatementNoShortIf withCond(java.util.Optional<Expression> cond) {
    return new WhileStatementNoShortIf(cond, body);
  }
  
  public WhileStatementNoShortIf withBody(StatementNoShortIf body) {
    return new WhileStatementNoShortIf(cond, body);
  }
}