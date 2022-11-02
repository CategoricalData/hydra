package hydra.ext.shex.syntax;

public class InlineShapeOr_ListOfSequence_Elmt {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeOr.ListOfSequence.Elmt");
  
  public final hydra.ext.shex.syntax.InlineShapeAnd inlineShapeAnd;
  
  public InlineShapeOr_ListOfSequence_Elmt (hydra.ext.shex.syntax.InlineShapeAnd inlineShapeAnd) {
    this.inlineShapeAnd = inlineShapeAnd;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeOr_ListOfSequence_Elmt)) {
      return false;
    }
    InlineShapeOr_ListOfSequence_Elmt o = (InlineShapeOr_ListOfSequence_Elmt) (other);
    return inlineShapeAnd.equals(o.inlineShapeAnd);
  }
  
  @Override
  public int hashCode() {
    return 2 * inlineShapeAnd.hashCode();
  }
}