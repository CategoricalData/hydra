package hydra.ext.shex.syntax;

public class MultiElementOneOf_ListOfSequence_Elmt {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.MultiElementOneOf.ListOfSequence.Elmt");
  
  public final hydra.ext.shex.syntax.GroupTripleExpr groupTripleExpr;
  
  public MultiElementOneOf_ListOfSequence_Elmt (hydra.ext.shex.syntax.GroupTripleExpr groupTripleExpr) {
    this.groupTripleExpr = groupTripleExpr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiElementOneOf_ListOfSequence_Elmt)) {
      return false;
    }
    MultiElementOneOf_ListOfSequence_Elmt o = (MultiElementOneOf_ListOfSequence_Elmt) (other);
    return groupTripleExpr.equals(o.groupTripleExpr);
  }
  
  @Override
  public int hashCode() {
    return 2 * groupTripleExpr.hashCode();
  }
}