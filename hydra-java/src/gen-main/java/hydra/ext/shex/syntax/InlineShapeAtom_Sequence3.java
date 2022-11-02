package hydra.ext.shex.syntax;

public class InlineShapeAtom_Sequence3 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeAtom.Sequence3");
  
  public final hydra.ext.shex.syntax.ShapeExpression shapeExpression;
  
  public InlineShapeAtom_Sequence3 (hydra.ext.shex.syntax.ShapeExpression shapeExpression) {
    this.shapeExpression = shapeExpression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeAtom_Sequence3)) {
      return false;
    }
    InlineShapeAtom_Sequence3 o = (InlineShapeAtom_Sequence3) (other);
    return shapeExpression.equals(o.shapeExpression);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeExpression.hashCode();
  }
}