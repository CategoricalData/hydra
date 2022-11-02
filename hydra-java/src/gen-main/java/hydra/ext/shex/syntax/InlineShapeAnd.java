package hydra.ext.shex.syntax;

public class InlineShapeAnd {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeAnd");
  
  public final hydra.ext.shex.syntax.InlineShapeNot inlineShapeNot;
  
  public final java.util.List<hydra.ext.shex.syntax.InlineShapeAnd_ListOfSequence_Elmt> listOfSequence;
  
  public InlineShapeAnd (hydra.ext.shex.syntax.InlineShapeNot inlineShapeNot, java.util.List<hydra.ext.shex.syntax.InlineShapeAnd_ListOfSequence_Elmt> listOfSequence) {
    this.inlineShapeNot = inlineShapeNot;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeAnd)) {
      return false;
    }
    InlineShapeAnd o = (InlineShapeAnd) (other);
    return inlineShapeNot.equals(o.inlineShapeNot) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * inlineShapeNot.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public InlineShapeAnd withInlineShapeNot(hydra.ext.shex.syntax.InlineShapeNot inlineShapeNot) {
    return new InlineShapeAnd(inlineShapeNot, listOfSequence);
  }
  
  public InlineShapeAnd withListOfSequence(java.util.List<hydra.ext.shex.syntax.InlineShapeAnd_ListOfSequence_Elmt> listOfSequence) {
    return new InlineShapeAnd(inlineShapeNot, listOfSequence);
  }
}