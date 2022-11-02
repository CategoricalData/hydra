package hydra.ext.shex.syntax;

public class ShapeAnd_ListOfSequence_Elmt {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeAnd.ListOfSequence.Elmt");
  
  public final hydra.ext.shex.syntax.ShapeNot shapeNot;
  
  public ShapeAnd_ListOfSequence_Elmt (hydra.ext.shex.syntax.ShapeNot shapeNot) {
    this.shapeNot = shapeNot;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeAnd_ListOfSequence_Elmt)) {
      return false;
    }
    ShapeAnd_ListOfSequence_Elmt o = (ShapeAnd_ListOfSequence_Elmt) (other);
    return shapeNot.equals(o.shapeNot);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeNot.hashCode();
  }
}