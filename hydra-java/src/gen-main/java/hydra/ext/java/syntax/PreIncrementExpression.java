package hydra.ext.java.syntax;

public class PreIncrementExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.PreIncrementExpression");
  
  public final hydra.ext.java.syntax.UnaryExpression value;
  
  public PreIncrementExpression (hydra.ext.java.syntax.UnaryExpression value) {
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