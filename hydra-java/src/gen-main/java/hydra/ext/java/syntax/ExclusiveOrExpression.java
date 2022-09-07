package hydra.ext.java.syntax;

public class ExclusiveOrExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ExclusiveOrExpression");
  
  public final java.util.List<hydra.ext.java.syntax.AndExpression> value;
  
  public ExclusiveOrExpression (java.util.List<hydra.ext.java.syntax.AndExpression> value) {
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