package hydra.ext.java.syntax;

public class PreIncrementExpression {
  public final UnaryExpression value;
  
  public PreIncrementExpression (UnaryExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PreIncrementExpression)) {
      return false;
    }
    PreIncrementExpression o = (PreIncrementExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}