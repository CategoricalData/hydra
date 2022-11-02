package hydra.ext.shex.syntax;

public class NodeConstraint_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.NodeConstraint.Sequence");
  
  public final java.util.List<hydra.ext.shex.syntax.XsFacet> listOfXsFacet;
  
  public NodeConstraint_Sequence (java.util.List<hydra.ext.shex.syntax.XsFacet> listOfXsFacet) {
    this.listOfXsFacet = listOfXsFacet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeConstraint_Sequence)) {
      return false;
    }
    NodeConstraint_Sequence o = (NodeConstraint_Sequence) (other);
    return listOfXsFacet.equals(o.listOfXsFacet);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfXsFacet.hashCode();
  }
}