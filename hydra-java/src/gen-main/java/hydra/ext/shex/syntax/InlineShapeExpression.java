package hydra.ext.shex.syntax;

public class InlineShapeExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeExpression");
  
  public final hydra.ext.shex.syntax.InlineShapeOr value;
  
  public InlineShapeExpression (hydra.ext.shex.syntax.InlineShapeOr value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeExpression)) {
      return false;
    }
    InlineShapeExpression o = (InlineShapeExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}