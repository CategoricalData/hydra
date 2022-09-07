package hydra.ext.java.syntax;

public class AndExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.AndExpression");
  
  public final java.util.List<hydra.ext.java.syntax.EqualityExpression> value;
  
  public AndExpression (java.util.List<hydra.ext.java.syntax.EqualityExpression> value) {
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