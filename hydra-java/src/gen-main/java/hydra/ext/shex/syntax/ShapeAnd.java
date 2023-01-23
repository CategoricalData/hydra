package hydra.ext.shex.syntax;

public class ShapeAnd {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeAnd");
  
  public final hydra.ext.shex.syntax.ShapeNot shapeNot;
  
  public final java.util.List<hydra.ext.shex.syntax.ShapeNot> listOfSequence;
  
  public ShapeAnd (hydra.ext.shex.syntax.ShapeNot shapeNot, java.util.List<hydra.ext.shex.syntax.ShapeNot> listOfSequence) {
    this.shapeNot = shapeNot;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeAnd)) {
      return false;
    }
    ShapeAnd o = (ShapeAnd) (other);
    return shapeNot.equals(o.shapeNot) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeNot.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public ShapeAnd withShapeNot(hydra.ext.shex.syntax.ShapeNot shapeNot) {
    return new ShapeAnd(shapeNot, listOfSequence);
  }
  
  public ShapeAnd withListOfSequence(java.util.List<hydra.ext.shex.syntax.ShapeNot> listOfSequence) {
    return new ShapeAnd(shapeNot, listOfSequence);
  }
}