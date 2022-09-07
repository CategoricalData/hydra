package hydra.ext.java.syntax;

public class IfThenStatement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.IfThenStatement");
  
  public final hydra.ext.java.syntax.Expression expression;
  
  public final hydra.ext.java.syntax.Statement statement;
  
  public IfThenStatement (hydra.ext.java.syntax.Expression expression, hydra.ext.java.syntax.Statement statement) {
    this.expression = expression;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfThenStatement)) {
      return false;
    }
    IfThenStatement o = (IfThenStatement) (other);
    return expression.equals(o.expression) && statement.equals(o.statement);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * statement.hashCode();
  }
  
  public IfThenStatement withExpression(hydra.ext.java.syntax.Expression expression) {
    return new IfThenStatement(expression, statement);
  }
  
  public IfThenStatement withStatement(hydra.ext.java.syntax.Statement statement) {
    return new IfThenStatement(expression, statement);
  }
}