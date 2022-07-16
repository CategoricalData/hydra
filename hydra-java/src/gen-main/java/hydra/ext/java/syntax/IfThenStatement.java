package hydra.ext.java.syntax;

public class IfThenStatement {
  public final Expression expression;
  
  public final Statement statement;
  
  public IfThenStatement (Expression expression, Statement statement) {
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
  
  public IfThenStatement withExpression(Expression expression) {
    return new IfThenStatement(expression, statement);
  }
  
  public IfThenStatement withStatement(Statement statement) {
    return new IfThenStatement(expression, statement);
  }
}