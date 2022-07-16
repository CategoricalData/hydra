package hydra.ext.java.syntax;

public class ConstantExpression {
  public final Expression value;
  
  public ConstantExpression (Expression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstantExpression)) {
      return false;
    }
    ConstantExpression o = (ConstantExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}