package hydra.ext.java.syntax;

public class WhileStatementNoShortIf {
  public final java.util.Optional<hydra.ext.java.syntax.Expression> cond;
  
  public final hydra.ext.java.syntax.StatementNoShortIf body;
  
  public WhileStatementNoShortIf (java.util.Optional<hydra.ext.java.syntax.Expression> cond, hydra.ext.java.syntax.StatementNoShortIf body) {
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
  
  public WhileStatementNoShortIf withCond(java.util.Optional<hydra.ext.java.syntax.Expression> cond) {
    return new WhileStatementNoShortIf(cond, body);
  }
  
  public WhileStatementNoShortIf withBody(hydra.ext.java.syntax.StatementNoShortIf body) {
    return new WhileStatementNoShortIf(cond, body);
  }
}