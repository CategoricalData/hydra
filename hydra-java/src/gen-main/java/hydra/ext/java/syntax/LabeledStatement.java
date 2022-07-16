package hydra.ext.java.syntax;

public class LabeledStatement {
  public final Identifier identifier;
  
  public final Statement statement;
  
  public LabeledStatement (Identifier identifier, Statement statement) {
    this.identifier = identifier;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledStatement)) {
      return false;
    }
    LabeledStatement o = (LabeledStatement) (other);
    return identifier.equals(o.identifier) && statement.equals(o.statement);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * statement.hashCode();
  }
  
  public LabeledStatement withIdentifier(Identifier identifier) {
    return new LabeledStatement(identifier, statement);
  }
  
  public LabeledStatement withStatement(Statement statement) {
    return new LabeledStatement(identifier, statement);
  }
}