package hydra.ext.java.syntax;

public class PreDecrementExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.PreDecrementExpression");
  
  public final hydra.ext.java.syntax.UnaryExpression value;
  
  public PreDecrementExpression (hydra.ext.java.syntax.UnaryExpression value) {
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