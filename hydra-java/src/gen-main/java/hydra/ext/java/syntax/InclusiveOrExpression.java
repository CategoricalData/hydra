package hydra.ext.java.syntax;

/**
 * Note: list cannot be empty
 */
public class InclusiveOrExpression {
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<ExclusiveOrExpression> value;
  
  public InclusiveOrExpression (java.util.List<ExclusiveOrExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InclusiveOrExpression)) {
      return false;
    }
    InclusiveOrExpression o = (InclusiveOrExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}