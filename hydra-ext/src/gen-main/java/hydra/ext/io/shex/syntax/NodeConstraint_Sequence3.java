// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NodeConstraint_Sequence3 implements Serializable, Comparable<NodeConstraint_Sequence3> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence3");
  
  public static final hydra.core.Name DATATYPE = new hydra.core.Name("Datatype");
  
  public static final hydra.core.Name LIST_OF_XS_FACET = new hydra.core.Name("listOfXsFacet");
  
  public final hydra.ext.io.shex.syntax.Datatype Datatype;
  
  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet;
  
  public NodeConstraint_Sequence3 (hydra.ext.io.shex.syntax.Datatype Datatype, hydra.util.ConsList<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet) {
    this.Datatype = Datatype;
    this.listOfXsFacet = listOfXsFacet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeConstraint_Sequence3)) {
      return false;
    }
    NodeConstraint_Sequence3 o = (NodeConstraint_Sequence3) other;
    return java.util.Objects.equals(
      this.Datatype,
      o.Datatype) && java.util.Objects.equals(
      this.listOfXsFacet,
      o.listOfXsFacet);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Datatype) + 3 * java.util.Objects.hashCode(listOfXsFacet);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeConstraint_Sequence3 other) {
    int cmp = 0;
    cmp = ((Comparable) Datatype).compareTo(other.Datatype);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) listOfXsFacet).compareTo(other.listOfXsFacet);
  }
  
  public NodeConstraint_Sequence3 withDatatype(hydra.ext.io.shex.syntax.Datatype Datatype) {
    return new NodeConstraint_Sequence3(Datatype, listOfXsFacet);
  }
  
  public NodeConstraint_Sequence3 withListOfXsFacet(hydra.util.ConsList<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet) {
    return new NodeConstraint_Sequence3(Datatype, listOfXsFacet);
  }
}
