package hydra.ext.shex.syntax;

public class InlineShapeAnd_ListOfSequence_Elmt {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeAnd.ListOfSequence.Elmt");
  
  public final hydra.ext.shex.syntax.InlineShapeNot inlineShapeNot;
  
  public InlineShapeAnd_ListOfSequence_Elmt (hydra.ext.shex.syntax.InlineShapeNot inlineShapeNot) {
    this.inlineShapeNot = inlineShapeNot;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeAnd_ListOfSequence_Elmt)) {
      return false;
    }
    InlineShapeAnd_ListOfSequence_Elmt o = (InlineShapeAnd_ListOfSequence_Elmt) (other);
    return inlineShapeNot.equals(o.inlineShapeNot);
  }
  
  @Override
  public int hashCode() {
    return 2 * inlineShapeNot.hashCode();
  }
}