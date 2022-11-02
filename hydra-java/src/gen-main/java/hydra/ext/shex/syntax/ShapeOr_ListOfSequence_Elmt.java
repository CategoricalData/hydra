package hydra.ext.shex.syntax;

public class ShapeOr_ListOfSequence_Elmt {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeOr.ListOfSequence.Elmt");
  
  public final hydra.ext.shex.syntax.ShapeAnd shapeAnd;
  
  public ShapeOr_ListOfSequence_Elmt (hydra.ext.shex.syntax.ShapeAnd shapeAnd) {
    this.shapeAnd = shapeAnd;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeOr_ListOfSequence_Elmt)) {
      return false;
    }
    ShapeOr_ListOfSequence_Elmt o = (ShapeOr_ListOfSequence_Elmt) (other);
    return shapeAnd.equals(o.shapeAnd);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeAnd.hashCode();
  }
}