package hydra.ext.shex.syntax;

public class InlineShapeOrRef_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeOrRef.Sequence");
  
  public final hydra.ext.shex.syntax.ShapeExprLabel shapeExprLabel;
  
  public InlineShapeOrRef_Sequence (hydra.ext.shex.syntax.ShapeExprLabel shapeExprLabel) {
    this.shapeExprLabel = shapeExprLabel;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeOrRef_Sequence)) {
      return false;
    }
    InlineShapeOrRef_Sequence o = (InlineShapeOrRef_Sequence) (other);
    return shapeExprLabel.equals(o.shapeExprLabel);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeExprLabel.hashCode();
  }
}