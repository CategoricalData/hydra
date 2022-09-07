package hydra.ext.java.syntax;

public class ExpressionStatement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ExpressionStatement");
  
  public final hydra.ext.java.syntax.StatementExpression value;
  
  public ExpressionStatement (hydra.ext.java.syntax.StatementExpression value) {
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