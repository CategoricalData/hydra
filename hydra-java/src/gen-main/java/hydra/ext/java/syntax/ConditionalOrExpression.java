package hydra.ext.java.syntax;

public class ConditionalOrExpression {
  public final java.util.List<hydra.ext.java.syntax.ConditionalAndExpression> value;
  
  public ConditionalOrExpression (java.util.List<hydra.ext.java.syntax.ConditionalAndExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConditionalOrExpression)) {
      return false;
    }
    ConditionalOrExpression o = (ConditionalOrExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}