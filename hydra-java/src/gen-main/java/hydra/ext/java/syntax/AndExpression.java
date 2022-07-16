package hydra.ext.java.syntax;

/**
 * Note: list cannot be empty
 */
public class AndExpression {
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<EqualityExpression> value;
  
  public AndExpression (java.util.List<EqualityExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AndExpression)) {
      return false;
    }
    AndExpression o = (AndExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}