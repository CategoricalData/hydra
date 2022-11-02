package hydra.ext.shex.syntax;

public class MultiElementGroup_ListOfSequence_Elmt {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.MultiElementGroup.ListOfSequence.Elmt");
  
  public final hydra.ext.shex.syntax.UnaryTripleExpr unaryTripleExpr;
  
  public MultiElementGroup_ListOfSequence_Elmt (hydra.ext.shex.syntax.UnaryTripleExpr unaryTripleExpr) {
    this.unaryTripleExpr = unaryTripleExpr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiElementGroup_ListOfSequence_Elmt)) {
      return false;
    }
    MultiElementGroup_ListOfSequence_Elmt o = (MultiElementGroup_ListOfSequence_Elmt) (other);
    return unaryTripleExpr.equals(o.unaryTripleExpr);
  }
  
  @Override
  public int hashCode() {
    return 2 * unaryTripleExpr.hashCode();
  }
}