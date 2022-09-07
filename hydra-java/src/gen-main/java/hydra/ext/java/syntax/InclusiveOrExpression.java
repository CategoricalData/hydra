package hydra.ext.java.syntax;

public class InclusiveOrExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.InclusiveOrExpression");
  
  public final java.util.List<hydra.ext.java.syntax.ExclusiveOrExpression> value;
  
  public InclusiveOrExpression (java.util.List<hydra.ext.java.syntax.ExclusiveOrExpression> value) {
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