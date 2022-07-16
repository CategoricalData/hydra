package hydra.ext.java.syntax;

/**
 * Note: list cannot be empty
 */
public class ExclusiveOrExpression {
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<AndExpression> value;
  
  public ExclusiveOrExpression (java.util.List<AndExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExclusiveOrExpression)) {
      return false;
    }
    ExclusiveOrExpression o = (ExclusiveOrExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}