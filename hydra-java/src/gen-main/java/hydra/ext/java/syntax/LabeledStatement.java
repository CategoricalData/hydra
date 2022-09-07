package hydra.ext.java.syntax;

public class LabeledStatement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.LabeledStatement");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.ext.java.syntax.Statement statement;
  
  public LabeledStatement (hydra.ext.java.syntax.Identifier identifier, hydra.ext.java.syntax.Statement statement) {
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
  
  public LabeledStatement withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new LabeledStatement(identifier, statement);
  }
  
  public LabeledStatement withStatement(hydra.ext.java.syntax.Statement statement) {
    return new LabeledStatement(identifier, statement);
  }
}