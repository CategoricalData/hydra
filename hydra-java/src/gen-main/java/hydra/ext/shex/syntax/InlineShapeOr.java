package hydra.ext.shex.syntax;

public class InlineShapeOr {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeOr");
  
  public final hydra.ext.shex.syntax.ShapeAnd shapeAnd;
  
  public final java.util.List<hydra.ext.shex.syntax.InlineShapeAnd> listOfSequence;
  
  public InlineShapeOr (hydra.ext.shex.syntax.ShapeAnd shapeAnd, java.util.List<hydra.ext.shex.syntax.InlineShapeAnd> listOfSequence) {
    this.shapeAnd = shapeAnd;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeOr)) {
      return false;
    }
    InlineShapeOr o = (InlineShapeOr) (other);
    return shapeAnd.equals(o.shapeAnd) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeAnd.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public InlineShapeOr withShapeAnd(hydra.ext.shex.syntax.ShapeAnd shapeAnd) {
    return new InlineShapeOr(shapeAnd, listOfSequence);
  }
  
  public InlineShapeOr withListOfSequence(java.util.List<hydra.ext.shex.syntax.InlineShapeAnd> listOfSequence) {
    return new InlineShapeOr(shapeAnd, listOfSequence);
  }
}