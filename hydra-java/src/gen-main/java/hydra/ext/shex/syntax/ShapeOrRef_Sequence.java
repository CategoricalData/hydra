package hydra.ext.shex.syntax;

public class ShapeOrRef_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeOrRef.Sequence");
  
  public final hydra.ext.shex.syntax.ShapeExprLabel shapeExprLabel;
  
  public ShapeOrRef_Sequence (hydra.ext.shex.syntax.ShapeExprLabel shapeExprLabel) {
    this.shapeExprLabel = shapeExprLabel;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeOrRef_Sequence)) {
      return false;
    }
    ShapeOrRef_Sequence o = (ShapeOrRef_Sequence) (other);
    return shapeExprLabel.equals(o.shapeExprLabel);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeExprLabel.hashCode();
  }
}