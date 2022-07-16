package hydra.ext.java.syntax;

/**
 * Note: list cannot be empty
 */
public class ConditionalOrExpression {
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<ConditionalAndExpression> value;
  
  public ConditionalOrExpression (java.util.List<ConditionalAndExpression> value) {
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