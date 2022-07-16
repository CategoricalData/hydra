package hydra.ext.java.syntax;

public class ExpressionStatement {
  public final StatementExpression value;
  
  public ExpressionStatement (StatementExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExpressionStatement)) {
      return false;
    }
    ExpressionStatement o = (ExpressionStatement) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}