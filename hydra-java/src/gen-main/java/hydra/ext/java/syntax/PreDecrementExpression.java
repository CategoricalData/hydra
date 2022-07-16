package hydra.ext.java.syntax;

public class PreDecrementExpression {
  public final UnaryExpression value;
  
  public PreDecrementExpression (UnaryExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PreDecrementExpression)) {
      return false;
    }
    PreDecrementExpression o = (PreDecrementExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}