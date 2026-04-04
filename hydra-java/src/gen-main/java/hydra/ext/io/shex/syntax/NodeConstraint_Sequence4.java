// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NodeConstraint_Sequence4 implements Serializable, Comparable<NodeConstraint_Sequence4> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence4");

  public static final hydra.core.Name VALUE_SET = new hydra.core.Name("ValueSet");

  public static final hydra.core.Name LIST_OF_XS_FACET = new hydra.core.Name("listOfXsFacet");

  public final hydra.ext.io.shex.syntax.ValueSet ValueSet;

  public final java.util.List<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet;

  public NodeConstraint_Sequence4 (hydra.ext.io.shex.syntax.ValueSet ValueSet, java.util.List<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet) {
    this.ValueSet = ValueSet;
    this.listOfXsFacet = listOfXsFacet;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeConstraint_Sequence4)) {
      return false;
    }
    NodeConstraint_Sequence4 o = (NodeConstraint_Sequence4) other;
    return java.util.Objects.equals(
      this.ValueSet,
      o.ValueSet) && java.util.Objects.equals(
      this.listOfXsFacet,
      o.listOfXsFacet);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ValueSet) + 3 * java.util.Objects.hashCode(listOfXsFacet);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeConstraint_Sequence4 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      ValueSet,
      other.ValueSet);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      listOfXsFacet,
      other.listOfXsFacet);
  }

  public NodeConstraint_Sequence4 withValueSet(hydra.ext.io.shex.syntax.ValueSet ValueSet) {
    return new NodeConstraint_Sequence4(ValueSet, listOfXsFacet);
  }

  public NodeConstraint_Sequence4 withListOfXsFacet(java.util.List<hydra.ext.io.shex.syntax.XsFacet> listOfXsFacet) {
    return new NodeConstraint_Sequence4(ValueSet, listOfXsFacet);
  }
}
