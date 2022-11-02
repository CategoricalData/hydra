package hydra.ext.shex.syntax;

public class ShapeExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeExpression");
  
  public final hydra.ext.shex.syntax.ShapeOr value;
  
  public ShapeExpression (hydra.ext.shex.syntax.ShapeOr value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeExpression)) {
      return false;
    }
    ShapeExpression o = (ShapeExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}