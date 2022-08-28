package hydra.ext.java.syntax;

public class ConditionalAndExpression {
  public final java.util.List<hydra.ext.java.syntax.InclusiveOrExpression> value;
  
  public ConditionalAndExpression (java.util.List<hydra.ext.java.syntax.InclusiveOrExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConditionalAndExpression)) {
      return false;
    }
    ConditionalAndExpression o = (ConditionalAndExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}