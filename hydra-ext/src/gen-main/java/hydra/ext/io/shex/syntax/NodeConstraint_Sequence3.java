// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NodeConstraint_Sequence3 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence3");
  
  public static final hydra.core.Name FIELD_NAME_DATATYPE = new hydra.core.Name("datatype");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_XS_FACET = new hydra.core.Name("listOfXsFacet");
  
  public final hydra.ext.io.shex.syntax.Datatype datatype;
  
  public final java.util.List<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet;
  
  public NodeConstraint_Sequence3 (hydra.ext.io.shex.syntax.Datatype datatype, java.util.List<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet) {
    java.util.Objects.requireNonNull((datatype));
    java.util.Objects.requireNonNull((listOfXsFacet));
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
  
  public NodeConstraint_Sequence3 withDatatype(hydra.ext.io.shex.syntax.Datatype datatype) {
    java.util.Objects.requireNonNull((datatype));
    return new NodeConstraint_Sequence3(datatype, listOfXsFacet);
  }
  
  public NodeConstraint_Sequence3 withListOfXsFacet(java.util.List<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet) {
    java.util.Objects.requireNonNull((listOfXsFacet));
    return new NodeConstraint_Sequence3(datatype, listOfXsFacet);
  }
}