package hydra.ext.java.syntax;

public class LabeledStatementNoShortIf {
  public final Identifier identifier;
  
  public final StatementNoShortIf statement;
  
  public LabeledStatementNoShortIf (Identifier identifier, StatementNoShortIf statement) {
    this.identifier = identifier;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledStatementNoShortIf)) {
      return false;
    }
    LabeledStatementNoShortIf o = (LabeledStatementNoShortIf) (other);
    return identifier.equals(o.identifier) && statement.equals(o.statement);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * statement.hashCode();
  }
  
  public LabeledStatementNoShortIf withIdentifier(Identifier identifier) {
    return new LabeledStatementNoShortIf(identifier, statement);
  }
  
  public LabeledStatementNoShortIf withStatement(StatementNoShortIf statement) {
    return new LabeledStatementNoShortIf(identifier, statement);
  }
}