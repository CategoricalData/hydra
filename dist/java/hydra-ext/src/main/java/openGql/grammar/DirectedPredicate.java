// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DirectedPredicate implements Serializable, Comparable<DirectedPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DirectedPredicate");

  public static final hydra.core.Name ELEMENT_VARIABLE_REFERENCE = new hydra.core.Name("elementVariableReference");

  public static final hydra.core.Name DIRECTED_PART = new hydra.core.Name("directedPart");

  public final String elementVariableReference;

  public final openGql.grammar.DirectedPredicatePart2 directedPart;

  public DirectedPredicate (String elementVariableReference, openGql.grammar.DirectedPredicatePart2 directedPart) {
    this.elementVariableReference = elementVariableReference;
    this.directedPart = directedPart;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DirectedPredicate)) {
      return false;
    }
    DirectedPredicate o = (DirectedPredicate) other;
    return java.util.Objects.equals(
      this.elementVariableReference,
      o.elementVariableReference) && java.util.Objects.equals(
      this.directedPart,
      o.directedPart);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(elementVariableReference) + 3 * java.util.Objects.hashCode(directedPart);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DirectedPredicate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      elementVariableReference,
      other.elementVariableReference);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      directedPart,
      other.directedPart);
  }

  public DirectedPredicate withElementVariableReference(String elementVariableReference) {
    return new DirectedPredicate(elementVariableReference, directedPart);
  }

  public DirectedPredicate withDirectedPart(openGql.grammar.DirectedPredicatePart2 directedPart) {
    return new DirectedPredicate(elementVariableReference, directedPart);
  }
}
