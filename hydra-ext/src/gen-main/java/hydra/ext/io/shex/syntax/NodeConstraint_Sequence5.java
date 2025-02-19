// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NodeConstraint_Sequence5 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence5");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_SET = new hydra.core.Name("valueSet");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_XS_FACET = new hydra.core.Name("listOfXsFacet");
  
  public final hydra.ext.io.shex.syntax.ValueSet valueSet;
  
  public final java.util.List<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet;
  
  public NodeConstraint_Sequence5 (hydra.ext.io.shex.syntax.ValueSet valueSet, java.util.List<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet) {
    java.util.Objects.requireNonNull((valueSet));
    java.util.Objects.requireNonNull((listOfXsFacet));
    this.valueSet = valueSet;
    this.listOfXsFacet = listOfXsFacet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeConstraint_Sequence5)) {
      return false;
    }
    NodeConstraint_Sequence5 o = (NodeConstraint_Sequence5) (other);
    return valueSet.equals(o.valueSet) && listOfXsFacet.equals(o.listOfXsFacet);
  }
  
  @Override
  public int hashCode() {
    return 2 * valueSet.hashCode() + 3 * listOfXsFacet.hashCode();
  }
  
  public NodeConstraint_Sequence5 withValueSet(hydra.ext.io.shex.syntax.ValueSet valueSet) {
    java.util.Objects.requireNonNull((valueSet));
    return new NodeConstraint_Sequence5(valueSet, listOfXsFacet);
  }
  
  public NodeConstraint_Sequence5 withListOfXsFacet(java.util.List<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet) {
    java.util.Objects.requireNonNull((listOfXsFacet));
    return new NodeConstraint_Sequence5(valueSet, listOfXsFacet);
  }
}