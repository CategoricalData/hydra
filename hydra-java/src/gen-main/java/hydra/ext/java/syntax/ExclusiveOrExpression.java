package hydra.ext.java.syntax;

public class ExclusiveOrExpression {
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