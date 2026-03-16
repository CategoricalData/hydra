// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NodeConstraint_Sequence2 implements Serializable, Comparable<NodeConstraint_Sequence2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence2");
  
  public static final hydra.core.Name NON_LITERAL_KIND = new hydra.core.Name("NonLiteralKind");
  
  public static final hydra.core.Name LIST_OF_STRING_FACET = new hydra.core.Name("listOfStringFacet");
  
  public final hydra.ext.io.shex.syntax.NonLiteralKind NonLiteralKind;
  
  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.StringFacet> listOfStringFacet;
  
  public NodeConstraint_Sequence2 (hydra.ext.io.shex.syntax.NonLiteralKind NonLiteralKind, hydra.util.ConsList<hydra.ext.io.shex.syntax.StringFacet> listOfStringFacet) {
    this.NonLiteralKind = NonLiteralKind;
    this.listOfStringFacet = listOfStringFacet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeConstraint_Sequence2)) {
      return false;
    }
    NodeConstraint_Sequence2 o = (NodeConstraint_Sequence2) other;
    return java.util.Objects.equals(
      this.NonLiteralKind,
      o.NonLiteralKind) && java.util.Objects.equals(
      this.listOfStringFacet,
      o.listOfStringFacet);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(NonLiteralKind) + 3 * java.util.Objects.hashCode(listOfStringFacet);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeConstraint_Sequence2 other) {
    int cmp = 0;
    cmp = ((Comparable) NonLiteralKind).compareTo(other.NonLiteralKind);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) listOfStringFacet).compareTo(other.listOfStringFacet);
  }
  
  public NodeConstraint_Sequence2 withNonLiteralKind(hydra.ext.io.shex.syntax.NonLiteralKind NonLiteralKind) {
    return new NodeConstraint_Sequence2(NonLiteralKind, listOfStringFacet);
  }
  
  public NodeConstraint_Sequence2 withListOfStringFacet(hydra.util.ConsList<hydra.ext.io.shex.syntax.StringFacet> listOfStringFacet) {
    return new NodeConstraint_Sequence2(NonLiteralKind, listOfStringFacet);
  }
}
