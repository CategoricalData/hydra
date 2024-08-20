// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class NodeConstraint_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.NodeConstraint.Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_NON_LITERAL_KIND = new hydra.core.Name("nonLiteralKind");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_STRING_FACET = new hydra.core.Name("listOfStringFacet");
  
  public final hydra.ext.shex.syntax.NonLiteralKind nonLiteralKind;
  
  public final java.util.List<hydra.ext.shex.syntax.StringFacet> listOfStringFacet;
  
  public NodeConstraint_Sequence2 (hydra.ext.shex.syntax.NonLiteralKind nonLiteralKind, java.util.List<hydra.ext.shex.syntax.StringFacet> listOfStringFacet) {
    java.util.Objects.requireNonNull((nonLiteralKind));
    java.util.Objects.requireNonNull((listOfStringFacet));
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
    java.util.Objects.requireNonNull((nonLiteralKind));
    return new NodeConstraint_Sequence2(nonLiteralKind, listOfStringFacet);
  }
  
  public NodeConstraint_Sequence2 withListOfStringFacet(java.util.List<hydra.ext.shex.syntax.StringFacet> listOfStringFacet) {
    java.util.Objects.requireNonNull((listOfStringFacet));
    return new NodeConstraint_Sequence2(nonLiteralKind, listOfStringFacet);
  }
}
