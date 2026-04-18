// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class IsLabelExpression implements Serializable, Comparable<IsLabelExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.IsLabelExpression");

  public static final hydra.core.Name IS_OR_COLON = new hydra.core.Name("isOrColon");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public final openGql.grammar.IsOrColon isOrColon;

  public final openGql.grammar.LabelExpression label;

  public IsLabelExpression (openGql.grammar.IsOrColon isOrColon, openGql.grammar.LabelExpression label) {
    this.isOrColon = isOrColon;
    this.label = label;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IsLabelExpression)) {
      return false;
    }
    IsLabelExpression o = (IsLabelExpression) other;
    return java.util.Objects.equals(
      this.isOrColon,
      o.isOrColon) && java.util.Objects.equals(
      this.label,
      o.label);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(isOrColon) + 3 * java.util.Objects.hashCode(label);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IsLabelExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      isOrColon,
      other.isOrColon);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      label,
      other.label);
  }

  public IsLabelExpression withIsOrColon(openGql.grammar.IsOrColon isOrColon) {
    return new IsLabelExpression(isOrColon, label);
  }

  public IsLabelExpression withLabel(openGql.grammar.LabelExpression label) {
    return new IsLabelExpression(isOrColon, label);
  }
}
