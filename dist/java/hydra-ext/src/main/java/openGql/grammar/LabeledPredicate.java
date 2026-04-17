// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class LabeledPredicate implements Serializable, Comparable<LabeledPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LabeledPredicate");

  public static final hydra.core.Name ELEMENT_VARIABLE_REFERENCE = new hydra.core.Name("elementVariableReference");

  public static final hydra.core.Name LABELED_PART = new hydra.core.Name("labeledPart");

  public final String elementVariableReference;

  public final openGql.grammar.LabeledPredicatePart2 labeledPart;

  public LabeledPredicate (String elementVariableReference, openGql.grammar.LabeledPredicatePart2 labeledPart) {
    this.elementVariableReference = elementVariableReference;
    this.labeledPart = labeledPart;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledPredicate)) {
      return false;
    }
    LabeledPredicate o = (LabeledPredicate) other;
    return java.util.Objects.equals(
      this.elementVariableReference,
      o.elementVariableReference) && java.util.Objects.equals(
      this.labeledPart,
      o.labeledPart);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(elementVariableReference) + 3 * java.util.Objects.hashCode(labeledPart);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LabeledPredicate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      elementVariableReference,
      other.elementVariableReference);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      labeledPart,
      other.labeledPart);
  }

  public LabeledPredicate withElementVariableReference(String elementVariableReference) {
    return new LabeledPredicate(elementVariableReference, labeledPart);
  }

  public LabeledPredicate withLabeledPart(openGql.grammar.LabeledPredicatePart2 labeledPart) {
    return new LabeledPredicate(elementVariableReference, labeledPart);
  }
}
