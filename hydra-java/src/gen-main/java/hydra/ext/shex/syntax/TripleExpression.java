package hydra.ext.shex.syntax;

public class TripleExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.TripleExpression");
  
  public final hydra.ext.shex.syntax.OneOfTripleExpr value;
  
  public TripleExpression (hydra.ext.shex.syntax.OneOfTripleExpr value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TripleExpression)) {
      return false;
    }
    TripleExpression o = (TripleExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}