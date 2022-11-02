package hydra.ext.shex.syntax;

public class MultiElementGroup {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.MultiElementGroup");
  
  public final hydra.ext.shex.syntax.UnaryTripleExpr unaryTripleExpr;
  
  public final java.util.List<hydra.ext.shex.syntax.MultiElementGroup_ListOfSequence_Elmt> listOfSequence;
  
  public final java.util.Optional<java.lang.Void> semi;
  
  public MultiElementGroup (hydra.ext.shex.syntax.UnaryTripleExpr unaryTripleExpr, java.util.List<hydra.ext.shex.syntax.MultiElementGroup_ListOfSequence_Elmt> listOfSequence, java.util.Optional<java.lang.Void> semi) {
    this.unaryTripleExpr = unaryTripleExpr;
    this.listOfSequence = listOfSequence;
    this.semi = semi;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiElementGroup)) {
      return false;
    }
    MultiElementGroup o = (MultiElementGroup) (other);
    return unaryTripleExpr.equals(o.unaryTripleExpr) && listOfSequence.equals(o.listOfSequence) && semi.equals(o.semi);
  }
  
  @Override
  public int hashCode() {
    return 2 * unaryTripleExpr.hashCode() + 3 * listOfSequence.hashCode() + 5 * semi.hashCode();
  }
  
  public MultiElementGroup withUnaryTripleExpr(hydra.ext.shex.syntax.UnaryTripleExpr unaryTripleExpr) {
    return new MultiElementGroup(unaryTripleExpr, listOfSequence, semi);
  }
  
  public MultiElementGroup withListOfSequence(java.util.List<hydra.ext.shex.syntax.MultiElementGroup_ListOfSequence_Elmt> listOfSequence) {
    return new MultiElementGroup(unaryTripleExpr, listOfSequence, semi);
  }
  
  public MultiElementGroup withSemi(java.util.Optional<java.lang.Void> semi) {
    return new MultiElementGroup(unaryTripleExpr, listOfSequence, semi);
  }
}