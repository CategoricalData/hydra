package hydra.ext.shex.syntax;

public class NodeConstraint_Sequence2 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.NodeConstraint.Sequence2");
  
  public final hydra.ext.shex.syntax.NonLiteralKind nonLiteralKind;
  
  public final java.util.List<hydra.ext.shex.syntax.StringFacet> listOfStringFacet;
  
  public NodeConstraint_Sequence2 (hydra.ext.shex.syntax.NonLiteralKind nonLiteralKind, java.util.List<hydra.ext.shex.syntax.StringFacet> listOfStringFacet) {
    this.nonLiteralKind = nonLiteralKind;
    this.listOfStringFacet = listOfStringFacet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeConstraint_Sequence2)) {
      return false;
    }
    NodeConstraint_Sequence2 o = (NodeConstraint_Sequence2) (other);
    return nonLiteralKind.equals(o.nonLiteralKind) && listOfStringFacet.equals(o.listOfStringFacet);
  }
  
  @Override
  public int hashCode() {
    return 2 * nonLiteralKind.hashCode() + 3 * listOfStringFacet.hashCode();
  }
  
  public NodeConstraint_Sequence2 withNonLiteralKind(hydra.ext.shex.syntax.NonLiteralKind nonLiteralKind) {
    return new NodeConstraint_Sequence2(nonLiteralKind, listOfStringFacet);
  }
  
  public NodeConstraint_Sequence2 withListOfStringFacet(java.util.List<hydra.ext.shex.syntax.StringFacet> listOfStringFacet) {
    return new NodeConstraint_Sequence2(nonLiteralKind, listOfStringFacet);
  }
}