package hydra.ext.shex.syntax;

public class ShapeAtom_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeAtom.Sequence2");
  
  public final hydra.ext.shex.syntax.ShapeExpression shapeExpression;
  
  public ShapeAtom_Sequence2 (hydra.ext.shex.syntax.ShapeExpression shapeExpression) {
    this.shapeExpression = shapeExpression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeAtom_Sequence2)) {
      return false;
    }
    ShapeAtom_Sequence2 o = (ShapeAtom_Sequence2) (other);
    return shapeExpression.equals(o.shapeExpression);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeExpression.hashCode();
  }
}