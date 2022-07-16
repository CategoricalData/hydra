package hydra.ext.java.syntax;

/**
 * Note: list cannot be empty
 */
public class ConditionalAndExpression {
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<InclusiveOrExpression> value;
  
  public ConditionalAndExpression (java.util.List<InclusiveOrExpression> value) {
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