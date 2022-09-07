package hydra.ext.java.syntax;

public class PostDecrementExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.PostDecrementExpression");
  
  public final hydra.ext.java.syntax.PostfixExpression value;
  
  public PostDecrementExpression (hydra.ext.java.syntax.PostfixExpression value) {
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