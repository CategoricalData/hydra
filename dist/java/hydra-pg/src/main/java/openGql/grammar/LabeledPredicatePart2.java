// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class LabeledPredicatePart2 implements Serializable, Comparable<LabeledPredicatePart2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LabeledPredicatePart2");

  public static final hydra.core.Name IS_LABELED_OR_COLON = new hydra.core.Name("isLabeledOrColon");

  public static final hydra.core.Name LABEL_EXPRESSION = new hydra.core.Name("labelExpression");

  public final openGql.grammar.IsLabeledOrColon isLabeledOrColon;

  public final openGql.grammar.LabelExpression labelExpression;

  public LabeledPredicatePart2 (openGql.grammar.IsLabeledOrColon isLabeledOrColon, openGql.grammar.LabelExpression labelExpression) {
    this.isLabeledOrColon = isLabeledOrColon;
    this.labelExpression = labelExpression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledPredicatePart2)) {
      return false;
    }
    LabeledPredicatePart2 o = (LabeledPredicatePart2) other;
    return java.util.Objects.equals(
      this.isLabeledOrColon,
      o.isLabeledOrColon) && java.util.Objects.equals(
      this.labelExpression,
      o.labelExpression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(isLabeledOrColon) + 3 * java.util.Objects.hashCode(labelExpression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LabeledPredicatePart2 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      isLabeledOrColon,
      other.isLabeledOrColon);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      labelExpression,
      other.labelExpression);
  }

  public LabeledPredicatePart2 withIsLabeledOrColon(openGql.grammar.IsLabeledOrColon isLabeledOrColon) {
    return new LabeledPredicatePart2(isLabeledOrColon, labelExpression);
  }

  public LabeledPredicatePart2 withLabelExpression(openGql.grammar.LabelExpression labelExpression) {
    return new LabeledPredicatePart2(isLabeledOrColon, labelExpression);
  }
}
