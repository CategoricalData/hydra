package hydra.ext.shex.syntax;

public class NodeConstraint_Sequence3 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.NodeConstraint.Sequence3");
  
  public final hydra.ext.shex.syntax.Datatype datatype;
  
  public final java.util.List<hydra.ext.shex.syntax.XsFacet> listOfXsFacet;
  
  public NodeConstraint_Sequence3 (hydra.ext.shex.syntax.Datatype datatype, java.util.List<hydra.ext.shex.syntax.XsFacet> listOfXsFacet) {
    this.datatype = datatype;
    this.listOfXsFacet = listOfXsFacet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeConstraint_Sequence3)) {
      return false;
    }
    NodeConstraint_Sequence3 o = (NodeConstraint_Sequence3) (other);
    return datatype.equals(o.datatype) && listOfXsFacet.equals(o.listOfXsFacet);
  }
  
  @Override
  public int hashCode() {
    return 2 * datatype.hashCode() + 3 * listOfXsFacet.hashCode();
  }
  
  public NodeConstraint_Sequence3 withDatatype(hydra.ext.shex.syntax.Datatype datatype) {
    return new NodeConstraint_Sequence3(datatype, listOfXsFacet);
  }
  
  public NodeConstraint_Sequence3 withListOfXsFacet(java.util.List<hydra.ext.shex.syntax.XsFacet> listOfXsFacet) {
    return new NodeConstraint_Sequence3(datatype, listOfXsFacet);
  }
}