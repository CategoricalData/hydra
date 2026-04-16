// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class IsOrColonWithLabels implements Serializable, Comparable<IsOrColonWithLabels> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.IsOrColonWithLabels");

  public static final hydra.core.Name IS_OR_COLON = new hydra.core.Name("isOrColon");

  public static final hydra.core.Name LABELS = new hydra.core.Name("labels");

  public final openGql.grammar.IsOrColon isOrColon;

  public final java.util.List<String> labels;

  public IsOrColonWithLabels (openGql.grammar.IsOrColon isOrColon, java.util.List<String> labels) {
    this.isOrColon = isOrColon;
    this.labels = labels;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IsOrColonWithLabels)) {
      return false;
    }
    IsOrColonWithLabels o = (IsOrColonWithLabels) other;
    return java.util.Objects.equals(
      this.isOrColon,
      o.isOrColon) && java.util.Objects.equals(
      this.labels,
      o.labels);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(isOrColon) + 3 * java.util.Objects.hashCode(labels);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IsOrColonWithLabels other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      isOrColon,
      other.isOrColon);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      labels,
      other.labels);
  }

  public IsOrColonWithLabels withIsOrColon(openGql.grammar.IsOrColon isOrColon) {
    return new IsOrColonWithLabels(isOrColon, labels);
  }

  public IsOrColonWithLabels withLabels(java.util.List<String> labels) {
    return new IsOrColonWithLabels(isOrColon, labels);
  }
}
