package hydra.ext.shex.syntax;

public class MultiElementOneOf {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.MultiElementOneOf");
  
  public final hydra.ext.shex.syntax.GroupTripleExpr groupTripleExpr;
  
  public final java.util.List<hydra.ext.shex.syntax.MultiElementOneOf_ListOfSequence_Elmt> listOfSequence;
  
  public MultiElementOneOf (hydra.ext.shex.syntax.GroupTripleExpr groupTripleExpr, java.util.List<hydra.ext.shex.syntax.MultiElementOneOf_ListOfSequence_Elmt> listOfSequence) {
    this.groupTripleExpr = groupTripleExpr;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiElementOneOf)) {
      return false;
    }
    MultiElementOneOf o = (MultiElementOneOf) (other);
    return groupTripleExpr.equals(o.groupTripleExpr) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * groupTripleExpr.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public MultiElementOneOf withGroupTripleExpr(hydra.ext.shex.syntax.GroupTripleExpr groupTripleExpr) {
    return new MultiElementOneOf(groupTripleExpr, listOfSequence);
  }
  
  public MultiElementOneOf withListOfSequence(java.util.List<hydra.ext.shex.syntax.MultiElementOneOf_ListOfSequence_Elmt> listOfSequence) {
    return new MultiElementOneOf(groupTripleExpr, listOfSequence);
  }
}