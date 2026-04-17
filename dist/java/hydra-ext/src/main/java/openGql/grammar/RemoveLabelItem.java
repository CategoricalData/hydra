// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class RemoveLabelItem implements Serializable, Comparable<RemoveLabelItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.RemoveLabelItem");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name IS_OR_COLON = new hydra.core.Name("isOrColon");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public final String variable;

  public final openGql.grammar.IsOrColon isOrColon;

  public final String label;

  public RemoveLabelItem (String variable, openGql.grammar.IsOrColon isOrColon, String label) {
    this.variable = variable;
    this.isOrColon = isOrColon;
    this.label = label;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RemoveLabelItem)) {
      return false;
    }
    RemoveLabelItem o = (RemoveLabelItem) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.isOrColon,
      o.isOrColon) && java.util.Objects.equals(
      this.label,
      o.label);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(isOrColon) + 5 * java.util.Objects.hashCode(label);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RemoveLabelItem other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variable,
      other.variable);
    if (cmp != 0) {
      return cmp;
    }
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

  public RemoveLabelItem withVariable(String variable) {
    return new RemoveLabelItem(variable, isOrColon, label);
  }

  public RemoveLabelItem withIsOrColon(openGql.grammar.IsOrColon isOrColon) {
    return new RemoveLabelItem(variable, isOrColon, label);
  }

  public RemoveLabelItem withLabel(String label) {
    return new RemoveLabelItem(variable, isOrColon, label);
  }
}
