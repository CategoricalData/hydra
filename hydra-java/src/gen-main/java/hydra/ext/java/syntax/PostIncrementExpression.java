package hydra.ext.java.syntax;

public class PostIncrementExpression {
  public final PostfixExpression value;
  
  public PostIncrementExpression (PostfixExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PostIncrementExpression)) {
      return false;
    }
    PostIncrementExpression o = (PostIncrementExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}