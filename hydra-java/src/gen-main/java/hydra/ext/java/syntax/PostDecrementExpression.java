package hydra.ext.java.syntax;

public class PostDecrementExpression {
  public final PostfixExpression value;
  
  public PostDecrementExpression (PostfixExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PostDecrementExpression)) {
      return false;
    }
    PostDecrementExpression o = (PostDecrementExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}