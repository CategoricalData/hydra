// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class NodeConstraint_Sequence5 implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.NodeConstraint.Sequence5");
  
  public final hydra.langs.shex.syntax.ValueSet valueSet;
  
  public final java.util.List<hydra.langs.shex.syntax.XsFacet> listOfXsFacet;
  
  public NodeConstraint_Sequence5 (hydra.langs.shex.syntax.ValueSet valueSet, java.util.List<hydra.langs.shex.syntax.XsFacet> listOfXsFacet) {
    if (valueSet == null) {
      throw new IllegalArgumentException("null value for 'valueSet' argument");
    }
    if (listOfXsFacet == null) {
      throw new IllegalArgumentException("null value for 'listOfXsFacet' argument");
    }
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
  
  public NodeConstraint_Sequence5 withValueSet(hydra.langs.shex.syntax.ValueSet valueSet) {
    if (valueSet == null) {
      throw new IllegalArgumentException("null value for 'valueSet' argument");
    }
    return new NodeConstraint_Sequence5(valueSet, listOfXsFacet);
  }
  
  public NodeConstraint_Sequence5 withListOfXsFacet(java.util.List<hydra.langs.shex.syntax.XsFacet> listOfXsFacet) {
    if (listOfXsFacet == null) {
      throw new IllegalArgumentException("null value for 'listOfXsFacet' argument");
    }
    return new NodeConstraint_Sequence5(valueSet, listOfXsFacet);
  }
}