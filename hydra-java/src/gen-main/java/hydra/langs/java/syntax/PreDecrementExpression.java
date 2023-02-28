package hydra.langs.java.syntax;

public class PreDecrementExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.PreDecrementExpression");
  
  public final hydra.langs.java.syntax.UnaryExpression value;
  
  public PreDecrementExpression (hydra.langs.java.syntax.UnaryExpression value) {
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