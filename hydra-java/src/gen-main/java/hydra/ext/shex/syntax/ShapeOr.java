package hydra.ext.shex.syntax;

public class ShapeOr {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeOr");
  
  public final hydra.ext.shex.syntax.ShapeAnd shapeAnd;
  
  public final java.util.List<hydra.ext.shex.syntax.ShapeAnd> listOfSequence;
  
  public ShapeOr (hydra.ext.shex.syntax.ShapeAnd shapeAnd, java.util.List<hydra.ext.shex.syntax.ShapeAnd> listOfSequence) {
    this.shapeAnd = shapeAnd;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeOr)) {
      return false;
    }
    ShapeOr o = (ShapeOr) (other);
    return shapeAnd.equals(o.shapeAnd) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeAnd.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public ShapeOr withShapeAnd(hydra.ext.shex.syntax.ShapeAnd shapeAnd) {
    return new ShapeOr(shapeAnd, listOfSequence);
  }
  
  public ShapeOr withListOfSequence(java.util.List<hydra.ext.shex.syntax.ShapeAnd> listOfSequence) {
    return new ShapeOr(shapeAnd, listOfSequence);
  }
}